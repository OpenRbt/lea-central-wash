package dal

import (
	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/goose"
	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/migration"
	"github.com/jmoiron/sqlx"

	"github.com/powerman/gotest/testinit"
)

func init() { testinit.Setup(2, setupIntegration) }

var testRepo *repo

func setupIntegration() {
	const migrationDir = "../migration"
	db, err := migration.Connect(ctx, migration.TestDBConfig())
	if err != nil {
		testinit.Fatal(err)
	}
	err = goose.UpTo(db, migrationDir, migration.CurrentVersion)
	if err != nil {
		testinit.Fatal(err)
	}

	testRepo = New(sqlx.NewDb(db, "postgres"), sqlx.NewDb(db, "postgres"))
	testinit.Teardown(testRepo.Close)
}

func (r *repo) truncate() error {
	_, err := r.db.Exec(`
do $$
DECLARE
	seq_name text; 
    statements CURSOR FOR
        SELECT tablename FROM pg_tables
        WHERE schemaname = 'public';
BEGIN
    FOR stmt IN statements LOOP
        EXECUTE 'TRUNCATE TABLE ' || quote_ident(stmt.tablename) || ' CASCADE;';
    END LOOP;

	FOR seq_name IN (SELECT sequence_name FROM information_schema.sequences WHERE sequence_schema = 'public') LOOP 
	   EXECUTE 'SELECT setval(' || quote_literal(seq_name) || ', 1, false)'; 
	END LOOP;

END $$ LANGUAGE plpgsql;
	`)
	return err
}
