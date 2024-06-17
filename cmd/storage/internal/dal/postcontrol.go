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
	if byStationID {
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
	if byStationID {
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

func (r *repo) GetListTasks(filter app.TaskFilter) ([]app.Task, int64, error) {
	var count int64
	var resTasks []resTask

	err := r.tx(ctx, nil, func(tx *sqlxx.Tx) error {
		err := tx.NamedSelectContext(ctx, &resTasks, sqlGetListTasks.WithPagination(filter.Pagination).String(), dalTaskFilter(filter))
		if err != nil {
			return err
		}

		if len(resTasks) > 0 {
			count = resTasks[0].TotalCount
		} else if filter.Offset() > 0 {
			return tx.NamedGetContext(ctx, &count, sqlGetListTasks.Count().String(), dalTaskFilter(filter))
		}

		return nil
	})
	if err != nil {
		return nil, 0, err
	}

	return appTasks(resTasks), count, nil
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
			ID:         id,
			Error:      updateTask.Error,
			RetryCount: updateTask.RetryCount,
			StartedAt:  updateTask.StartedAt,
			StoppedAt:  updateTask.StoppedAt,
			Status:     dalNullableTaskStatus(updateTask.Status),
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
