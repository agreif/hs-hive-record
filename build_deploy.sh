#/bin/sh

local_app_name=hiverec
app_user=wwwhiverec
deploy_dir=/var/lib/$app_user
dist_dir=`stack path --local-bin`

if test -f $dist_dir/$local_app_name; then
    rm $dist_dir/$local_app_name
fi

git pull

stack clean
stack install
if test $? -ne 0; then
    exit 1
fi

docroot_dir=$deploy_dir/docroot
docroot_new_dir=${docroot_dir}_new

if sudo test ! -L $docroot_dir; then
    sudo mkdir ${docroot_dir}_0
    sudo ln -s ${docroot_dir}_0 $docroot_dir
fi

if test -d ${docroot_new_dir}; then
    sudo rm -rf ${docroot_new_dir}
fi
sudo mkdir -p ${docroot_new_dir}
sudo cp -r \
     static \
     config \
     $dist_dir/$local_app_name \
     postgresql_setup.sql \
     ${docroot_new_dir}
sudo chown -R $app_user:$app_user ${docroot_new_dir}
sudo chmod -R u+w ${docroot_new_dir}

echo "to restart..."
echo "    sudo pg_dump -U postgres --file=$deploy_dir/docroot/db_hiverec_$(date +%Y%m%d_%H%M%S)_dropdbifexists_createdb_filldata.dump --if-exists --clean --create hiverec"
echo "    sleep 2"
echo "    sudo su -c 'chmod 440 $deploy_dir/docroot/db_*.dump'"
echo "    sudo systemctl restart $local_app_name"
echo "    sleep 5"
echo "    sudo ls -la $deploy_dir"
echo "    sleep 5"
echo "    sudo psql -U hiverec hiverec -c '\i $deploy_dir/docroot/postgresql_setup.sql'"
echo "    sleep 1"
echo "    cd /var/lib/wwwhiverec/docroot/static/js/riot"
echo "    sleep 1"
echo "    sudo ~/node_modules/.bin/riot . --output ."
