-- gen triggers - start




drop function public.process_audit_user() cascade;
create or replace function public.process_audit_user()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('user_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into user_history
                       (id, ident, password, email, is_admin, version, created_at, created_by, updated_at, updated_by)
                       values
                       (new.id, new.ident, new.password, new.email, new.is_admin, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by);
                return new;
            end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_user after insert or update on public.user for each row execute procedure public.process_audit_user();



drop function public.process_audit_config() cascade;
create or replace function public.process_audit_config()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('config_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into config_history
                       (id, code, string_value, int_value, double_value, bool_value, version, created_at, created_by, updated_at, updated_by)
                       values
                       (new.id, new.code, new.string_value, new.int_value, new.double_value, new.bool_value, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by);
                return new;
            end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_config after insert or update on public.config for each row execute procedure public.process_audit_config();




drop function public.process_audit_rawdata() cascade;
create or replace function public.process_audit_rawdata()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('rawdata_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into rawdata_history
                       (id, bytes, version, created_at, created_by, updated_at, updated_by)
                       values
                       (new.id, new.bytes, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by);
                return new;
            end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_rawdata after insert or update on public.rawdata for each row execute procedure public.process_audit_rawdata();



drop function public.process_audit_location() cascade;
create or replace function public.process_audit_location()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('location_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into location_history
                       (id, name, version, created_at, created_by, updated_at, updated_by)
                       values
                       (new.id, new.name, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by);
                return new;
            end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_location after insert or update on public.location for each row execute procedure public.process_audit_location();



drop function public.process_audit_hive() cascade;
create or replace function public.process_audit_hive()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('hive_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into hive_history
                       (id, location_id, name, description, version, created_at, created_by, updated_at, updated_by)
                       values
                       (new.id, new.location_id, new.name, new.description, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by);
                return new;
            end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_hive after insert or update on public.hive for each row execute procedure public.process_audit_hive();



drop function public.process_audit_inspection() cascade;
create or replace function public.process_audit_inspection()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('inspection_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into inspection_history
                       (id, hive_id, date, temper_type_id, running_type_id, swarming_type_id, queen_seen, total_frames, bee_covered_frames, brood_frames, pollen_frames, honey_frames, mite_fall, feeding, notes, version, created_at, created_by, updated_at, updated_by)
                       values
                       (new.id, new.hive_id, new.date, new.temper_type_id, new.running_type_id, new.swarming_type_id, new.queen_seen, new.total_frames, new.bee_covered_frames, new.brood_frames, new.pollen_frames, new.honey_frames, new.mite_fall, new.feeding, new.notes, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by);
                return new;
            end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_inspection after insert or update on public.inspection for each row execute procedure public.process_audit_inspection();



drop function public.process_audit_inspectionfile() cascade;
create or replace function public.process_audit_inspectionfile()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('inspectionfile_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into inspectionfile_history
                       (id, inspection_id, rawdata_id, filename, mimetype, size, version, created_at, created_by, updated_at, updated_by)
                       values
                       (new.id, new.inspection_id, new.rawdata_id, new.filename, new.mimetype, new.size, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by);
                return new;
            end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_inspectionfile after insert or update on public.inspectionfile for each row execute procedure public.process_audit_inspectionfile();



drop function public.process_audit_temper_type() cascade;
create or replace function public.process_audit_temper_type()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('temper_type_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into temper_type_history
                       (id, name, sort_index, version, created_at, created_by, updated_at, updated_by)
                       values
                       (new.id, new.name, new.sort_index, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by);
                return new;
            end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_temper_type after insert or update on public.temper_type for each row execute procedure public.process_audit_temper_type();



drop function public.process_audit_running_type() cascade;
create or replace function public.process_audit_running_type()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('running_type_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into running_type_history
                       (id, name, sort_index, version, created_at, created_by, updated_at, updated_by)
                       values
                       (new.id, new.name, new.sort_index, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by);
                return new;
            end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_running_type after insert or update on public.running_type for each row execute procedure public.process_audit_running_type();



drop function public.process_audit_swarming_type() cascade;
create or replace function public.process_audit_swarming_type()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('swarming_type_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into swarming_type_history
                       (id, name, sort_index, version, created_at, created_by, updated_at, updated_by)
                       values
                       (new.id, new.name, new.sort_index, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by);
                return new;
            end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_swarming_type after insert or update on public.swarming_type for each row execute procedure public.process_audit_swarming_type();

-- gen triggers - end
