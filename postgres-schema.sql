CREATE TABLE updates (
    id bigserial NOT NULL,
    action json NOT NULL
);

CREATE FUNCTION updates_notify() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
notify updates;
return new; end;
$$;

CREATE TRIGGER updates_insert_notify AFTER INSERT ON updates FOR EACH STATEMENT EXECUTE PROCEDURE updates_notify();
