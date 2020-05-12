#/bin/sh

local_app_name=hiverec
app_user=wwwhiverec
deploy_dir=/var/lib/$app_user
dist_dir=`stack path --local-bin`

if test -f $dist_dir/$local_app_name; then
    rm $dist_dir/$local_app_name
fi

git pull

# force new timestamp tag later
for f in static/js/riot/*.js; do
    echo "`date`" > $f
done

stack clean
stack install
if test $? -ne 0; then
    exit 1
fi

docroot_dir=$deploy_dir/docroot
docroot_new_dir=${docroot_dir}_new

echo
echo "DEPLOY ... "
echo

sudo -s <<EOF
if test ! -L $docroot_dir; then
    mkdir ${docroot_dir}_0
    ln -s ${docroot_dir}_0 $docroot_dir
fi

if test -d ${docroot_new_dir}; then
    rm -rf ${docroot_new_dir}
fi
mkdir -p ${docroot_new_dir}

echo "copy files to ${docroot_new_dir} ..."
cp -r \
     static \
     config \
     $dist_dir/hiverec* \
     postgresql_setup.sql \
     ${docroot_new_dir}

dump_file=$docroot_dir/db_hiverec_$(date +%Y%m%d_%H%M%S)_dropdbifexists_createdb_filldata.dump
echo "pg_dump to \$dump_file ..."
pg_dump -U postgres --file=\$dump_file --if-exists --clean --create hiverec
chmod 440 $docroot_dir/db_*.dump

echo "exec postgresql_setup.sql ..."
psql -U hiverec hiverec -c '\i $docroot_new_dir/postgresql_setup.sql'
sleep 1
if test ! -e /var/lib/$app_user/node_modules/.bin/riot; then
    npm --prefix /var/lib/$app_user install @riotjs/cli
fi

echo "hiverec-gen-riot-files ..."
approot=`systemctl show $local_app_name | grep APPROOT | sed -e 's|.*APPROOT=\([^ ]*\) .*|\1|'`
echo "APPROOT: \$approot"
sleep 1
(cd ${docroot_new_dir}; APPROOT=\$approot ./hiverec-gen-riot-files)

sleep 1
/var/lib/$app_user/node_modules/.bin/riot \
    $docroot_new_dir/static/js/riot \
    --output $docroot_new_dir/static/js/riot

sleep 1
chown -R $app_user:$app_user ${docroot_new_dir}
chmod -R u+w ${docroot_new_dir}

echo "restart ..."
systemctl restart $local_app_name
systemctl status $local_app_name
EOF
