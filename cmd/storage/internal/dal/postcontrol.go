package dal

import (
	"database/sql"
	"encoding/json"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/app"
	"github.com/powerman/sqlxx"
)

func (r *repo) GetListBuildScripts() ([]app.BuildScript, error) {
	var buildScripts []app.BuildScript

	err := r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		var res []resBuildScript
		err := tx.SelectContext(ctx, &res, sqlGetListBuildScripts)

		if err != nil {
			return err
		}

		buildScripts, err = appListBuildScripts(res)
		if err != nil {
			return err
		}

		return nil
	})

	return buildScripts, err
}

func (r *repo) GetBuildScript(id int) (app.BuildScript, error) {
	return r.getBuildScript(id, false)
}

func (r *repo) GetBuildScriptByStationID(id app.StationID) (app.BuildScript, error) {
	return r.getBuildScript(int(id), true)
}

func (r *repo) getBuildScript(id int, byStationID bool) (app.BuildScript, error) {
	var buildScript app.BuildScript

	sqlGet := sqlGetBuildScript
	if byStationID == true {
		sqlGet = sqlGetBuildScriptByStationID
	}

	err := r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		var res resBuildScript
		err := tx.NamedGetContext(ctx, &res, sqlGet, argGetBuildScript{
			ID: id,
		})

		if err != nil {
			if err == sql.ErrNoRows {
				return app.ErrNotFound
			}
			return err
		}

		buildScript, err = appBuildScript(res)
		return err
	})

	return buildScript, err
}

func (r *repo) CreateBuildScript(createBuildScript app.SetBuildScript) (app.BuildScript, error) {
	var buildScript app.BuildScript

	commands, err := json.Marshal(createBuildScript.Commands)
	if err != nil {
		return app.BuildScript{}, err
	}

	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		var res resBuildScript
		err := tx.NamedGetContext(ctx, &res, sqlInsertBuildScript, argInsertBuildScript{
			StationID: int(createBuildScript.StationID),
			Name:      createBuildScript.Name,
			Commands:  string(commands),
		})
		if err != nil {
			return err
		}

		buildScript, err = appBuildScript(res)
		if err != nil {
			return err
		}

		return nil
	})

	return buildScript, err
}

func (r *repo) UpdateBuildScript(id int, updateBuildScript app.SetBuildScript) (app.BuildScript, error) {
	var buildScript app.BuildScript

	commands, err := json.Marshal(updateBuildScript.Commands)
	if err != nil {
		return app.BuildScript{}, err
	}

	err = r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		var res resBuildScript
		err := tx.NamedGetContext(ctx, &res, sqlUpdateBuildScript, argUpdateBuildScript{
			ID:        id,
			StationID: int(updateBuildScript.StationID),
			Name:      updateBuildScript.Name,
			Commands:  string(commands),
		})

		if err != nil {
			if err == sql.ErrNoRows {
				return app.ErrNotFound
			}
			return err
		}

		buildScript, err = appBuildScript(res)
		if err != nil {
			return err
		}

		return nil
	})

	return buildScript, err
}

func (r *repo) DeleteBuildScriptByStationID(id app.StationID) error {
	return r.deleteBuildScript(int(id), true)
}

func (r *repo) DeleteBuildScript(id int) error {
	return r.deleteBuildScript(id, false)
}

func (r *repo) deleteBuildScript(id int, byStationID bool) error {
	sqlDelete := sqlDeleteBuildScript
	if byStationID == true {
		sqlDelete = sqlDeleteBuildScriptByStationID
	}

	err := r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		res, err := tx.NamedExecContext(ctx, sqlDelete, argGetBuildScript{
			ID: id,
		})

		if err != nil {
			return err
		}

		rows, err := res.RowsAffected()
		if err != nil {
			return err
		}
		if rows == 0 {
			return app.ErrNotFound
		}

		return nil
	})

	return err
}

func (r *repo) GetTask(id int) (app.Task, error) {
	var task app.Task

	err := r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		var res resTask
		err := tx.NamedGetContext(ctx, &res, sqlGetTask, argGetTask{
			ID: id,
		})

		if err != nil {
			if err == sql.ErrNoRows {
				return app.ErrNotFound
			}
			return err
		}

		task = appTask(res)

		return nil
	})

	return task, err
}

func (r *repo) GetListTasks(filter app.TasksFilter) (app.TaskPage, error) {
	count := 0
	var tasks []app.Task

	err := r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		var res []resTasks
		err := tx.NamedSelectContext(ctx, &res, sqlGetListTasks, argGetListTasks{
			StationsID: dalStationsId(filter.StationsID),
			Statuses:   dalTaskStatuses(filter.Statuses),
			Types:      dalTaskTypes(filter.Types),
			Sort:       dalTaskSort(filter.Sort),
			Limit:      filter.Limit(),
			Offset:     filter.Offset(),
		})
		if err != nil {
			return err
		}
		if len(res) > 0 {
			count = res[0].TotalCount
		}

		tasks = appListTasks(res)

		return nil
	})

	return app.NewPage(tasks, filter.Filter, count), err
}

func (r *repo) CreateTask(createTask app.CreateTask) (app.Task, error) {
	var task app.Task

	err := r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		var res resTask
		err := tx.NamedGetContext(ctx, &res, sqlInsertTask, argInsertTask{
			StationID: int(createTask.StationID),
			VersionID: createTask.VersionID,
			Type:      dalTaskType(createTask.Type),
		})

		if err != nil {
			return err
		}

		task = appTask(res)

		return nil
	})

	return task, err
}

func (r *repo) UpdateTask(id int, updateTask app.UpdateTask) (app.Task, error) {
	var task app.Task

	err := r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		var res resTask
		err := tx.NamedGetContext(ctx, &res, sqlUpdateTask, argUpdateTask{
			ID:        id,
			Error:     updateTask.Error,
			RetryCount: updateTask.RetryCount,
			StartedAt: updateTask.StartedAt,
			StoppedAt: updateTask.StoppedAt,
			Status:    dalNullableTaskStatus(updateTask.Status),
		})

		if err != nil {
			if err == sql.ErrNoRows {
				return app.ErrNotFound
			}
			return err
		}

		task = appTask(res)
		if err != nil {
			return err
		}

		return nil
	})

	return task, err
}

func (r *repo) DeleteTask(id int) error {
	err := r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		res, err := tx.NamedExecContext(ctx, sqlDeleteTask, argGetTask{
			ID: id,
		})

		if err != nil {
			return err
		}

		rows, err := res.RowsAffected()
		if err != nil {
			return err
		}
		if rows == 0 {
			return app.ErrNotFound
		}

		return nil
	})

	return err
}
