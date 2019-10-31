-- +goose Up
-- SQL in this section is executed when the migration is applied.
-- +goose StatementBegin
CREATE FUNCTION down_not_supported() RETURNS VOID AS $$
BEGIN
    RAISE EXCEPTION 'downgrade is not supported, restore from backup instead';
END $$
LANGUAGE PLPGSQL;
-- +goose StatementEnd

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.
-- select down_not_supported();
drop function down_not_supported;
