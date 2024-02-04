-- +goose Up
-- SQL in this section is executed when the migration is applied.

CREATE TABLE public.build_scripts (
	id          SERIAL PRIMARY KEY,
	station_id  INT    NOT NULL     REFERENCES station(id),
	name        TEXT   NOT NULL,
	commands    TEXT   NOT NULL
);

CREATE TYPE TASK_TYPE_ENUM AS ENUM ('build', 'update', 'reboot', 'get_versions', 'pull_firmware', 'set_version');
CREATE TYPE TASK_STATUS_ENUM AS ENUM ('queue', 'started', 'completed', 'error', 'canceled');

CREATE TABLE public.tasks (
	id          SERIAL 		  	 PRIMARY KEY,
	station_id  INT          	 NOT NULL     REFERENCES station(id),
	version_id  INT,
	type        TASK_TYPE_ENUM   NOT NULL,
	status 		TASK_STATUS_ENUM NOT NULL     DEFAULT 'queue',
	error 		TEXT,
	created_at	TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
	started_at	TIMESTAMP WITH TIME ZONE,
	stopped_at	TIMESTAMP WITH TIME ZONE
);

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

DROP TABLE IF EXISTS public.build_scripts;
DROP TABLE IF EXISTS public.tasks;
DROP TYPE  IF EXISTS TASK_TYPE_ENUM;
DROP TYPE  IF EXISTS TASK_STATUS_ENUM;
