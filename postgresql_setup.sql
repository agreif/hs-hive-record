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
                       (id, ident, password, email, is_admin, version, created_at, created_by, updated_at, updated_by, db_action)
                       values
                       (new.id, new.ident, new.password, new.email, new.is_admin, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by, TG_OP);
                return new;
           elsif (TG_OP = 'DELETE') then
                insert into user_history
                       (id, ident, password, email, is_admin, version, created_at, created_by, updated_at, updated_by, db_action)
                       values
                       (old.id, old.ident, old.password, old.email, old.is_admin, old.version, old.created_at, old.created_by, old.updated_at, old.updated_by, TG_OP);
               return old;
           end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_user after insert or update or delete on public.user for each row execute procedure public.process_audit_user();



drop function public.process_audit_config() cascade;
create or replace function public.process_audit_config()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('config_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into config_history
                       (id, code, string_value, int_value, double_value, bool_value, version, created_at, created_by, updated_at, updated_by, db_action)
                       values
                       (new.id, new.code, new.string_value, new.int_value, new.double_value, new.bool_value, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by, TG_OP);
                return new;
           elsif (TG_OP = 'DELETE') then
                insert into config_history
                       (id, code, string_value, int_value, double_value, bool_value, version, created_at, created_by, updated_at, updated_by, db_action)
                       values
                       (old.id, old.code, old.string_value, old.int_value, old.double_value, old.bool_value, old.version, old.created_at, old.created_by, old.updated_at, old.updated_by, TG_OP);
               return old;
           end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_config after insert or update or delete on public.config for each row execute procedure public.process_audit_config();





drop function public.process_audit_location() cascade;
create or replace function public.process_audit_location()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('location_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into location_history
                       (id, name, version, created_at, created_by, updated_at, updated_by, db_action)
                       values
                       (new.id, new.name, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by, TG_OP);
                return new;
           elsif (TG_OP = 'DELETE') then
                insert into location_history
                       (id, name, version, created_at, created_by, updated_at, updated_by, db_action)
                       values
                       (old.id, old.name, old.version, old.created_at, old.created_by, old.updated_at, old.updated_by, TG_OP);
               return old;
           end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_location after insert or update or delete on public.location for each row execute procedure public.process_audit_location();



drop function public.process_audit_hive() cascade;
create or replace function public.process_audit_hive()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('hive_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into hive_history
                       (id, location_id, name, queen_year, description, is_dissolved, version, created_at, created_by, updated_at, updated_by, db_action)
                       values
                       (new.id, new.location_id, new.name, new.queen_year, new.description, new.is_dissolved, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by, TG_OP);
                return new;
           elsif (TG_OP = 'DELETE') then
                insert into hive_history
                       (id, location_id, name, queen_year, description, is_dissolved, version, created_at, created_by, updated_at, updated_by, db_action)
                       values
                       (old.id, old.location_id, old.name, old.queen_year, old.description, old.is_dissolved, old.version, old.created_at, old.created_by, old.updated_at, old.updated_by, TG_OP);
               return old;
           end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_hive after insert or update or delete on public.hive for each row execute procedure public.process_audit_hive();



drop function public.process_audit_inspection() cascade;
create or replace function public.process_audit_inspection()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('inspection_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into inspection_history
                       (id, hive_id, date, swarming_type_id, queen_seen, bee_covered_frames, brood_frames, honey_frames, treatment, feeding, notes, version, created_at, created_by, updated_at, updated_by, db_action)
                       values
                       (new.id, new.hive_id, new.date, new.swarming_type_id, new.queen_seen, new.bee_covered_frames, new.brood_frames, new.honey_frames, new.treatment, new.feeding, new.notes, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by, TG_OP);
                return new;
           elsif (TG_OP = 'DELETE') then
                insert into inspection_history
                       (id, hive_id, date, swarming_type_id, queen_seen, bee_covered_frames, brood_frames, honey_frames, treatment, feeding, notes, version, created_at, created_by, updated_at, updated_by, db_action)
                       values
                       (old.id, old.hive_id, old.date, old.swarming_type_id, old.queen_seen, old.bee_covered_frames, old.brood_frames, old.honey_frames, old.treatment, old.feeding, old.notes, old.version, old.created_at, old.created_by, old.updated_at, old.updated_by, TG_OP);
               return old;
           end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_inspection after insert or update or delete on public.inspection for each row execute procedure public.process_audit_inspection();



drop function public.process_audit_inspectionfile() cascade;
create or replace function public.process_audit_inspectionfile()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('inspectionfile_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into inspectionfile_history
                       (id, inspection_id, rawdata_id, filename, mimetype, size, version, created_at, created_by, updated_at, updated_by, db_action)
                       values
                       (new.id, new.inspection_id, new.rawdata_id, new.filename, new.mimetype, new.size, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by, TG_OP);
                return new;
           elsif (TG_OP = 'DELETE') then
                insert into inspectionfile_history
                       (id, inspection_id, rawdata_id, filename, mimetype, size, version, created_at, created_by, updated_at, updated_by, db_action)
                       values
                       (old.id, old.inspection_id, old.rawdata_id, old.filename, old.mimetype, old.size, old.version, old.created_at, old.created_by, old.updated_at, old.updated_by, TG_OP);
               return old;
           end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_inspectionfile after insert or update or delete on public.inspectionfile for each row execute procedure public.process_audit_inspectionfile();



drop function public.process_audit_swarming_type() cascade;
create or replace function public.process_audit_swarming_type()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('swarming_type_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into swarming_type_history
                       (id, name, sort_index, version, created_at, created_by, updated_at, updated_by, db_action)
                       values
                       (new.id, new.name, new.sort_index, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by, TG_OP);
                return new;
           elsif (TG_OP = 'DELETE') then
                insert into swarming_type_history
                       (id, name, sort_index, version, created_at, created_by, updated_at, updated_by, db_action)
                       values
                       (old.id, old.name, old.sort_index, old.version, old.created_at, old.created_by, old.updated_at, old.updated_by, TG_OP);
               return old;
           end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_swarming_type after insert or update or delete on public.swarming_type for each row execute procedure public.process_audit_swarming_type();



drop function public.process_audit_note() cascade;
create or replace function public.process_audit_note()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('note_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into note_history
                       (id, date, text, version, created_at, created_by, updated_at, updated_by, db_action)
                       values
                       (new.id, new.date, new.text, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by, TG_OP);
                return new;
           elsif (TG_OP = 'DELETE') then
                insert into note_history
                       (id, date, text, version, created_at, created_by, updated_at, updated_by, db_action)
                       values
                       (old.id, old.date, old.text, old.version, old.created_at, old.created_by, old.updated_at, old.updated_by, TG_OP);
               return old;
           end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_note after insert or update or delete on public.note for each row execute procedure public.process_audit_note();



drop function public.process_audit_notefile() cascade;
create or replace function public.process_audit_notefile()
 returns trigger
 language plpgsql
as $function$
   begin
       if to_regclass('notefile_history') is not null then
           if (TG_OP = 'UPDATE' or TG_OP = 'INSERT') then
                insert into notefile_history
                       (id, note_id, rawdata_id, filename, mimetype, size, version, created_at, created_by, updated_at, updated_by, db_action)
                       values
                       (new.id, new.note_id, new.rawdata_id, new.filename, new.mimetype, new.size, new.version, new.created_at, new.created_by, new.updated_at, new.updated_by, TG_OP);
                return new;
           elsif (TG_OP = 'DELETE') then
                insert into notefile_history
                       (id, note_id, rawdata_id, filename, mimetype, size, version, created_at, created_by, updated_at, updated_by, db_action)
                       values
                       (old.id, old.note_id, old.rawdata_id, old.filename, old.mimetype, old.size, old.version, old.created_at, old.created_by, old.updated_at, old.updated_by, TG_OP);
               return old;
           end if;
       end if;
       return null; -- result is ignored since this is an after trigger
    end;
$function$;

create trigger audit_notefile after insert or update or delete on public.notefile for each row execute procedure public.process_audit_notefile();

-- gen triggers - end
