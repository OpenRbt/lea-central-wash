package dal

import (
	"testing"
	"time"

	"github.com/OpenRbt/lea-central-wash/cmd/storage/internal/app"
	"github.com/powerman/check"
)

func TestCreateBuildScript(tt *testing.T) {
	t := check.T(tt)
	t.Nil(testRepo.truncate())
	addTestData(t)

	bs, err := testRepo.CreateBuildScript(testBuildScript)
	t.Nil(err)
	t.Equal(bs.StationID, testBuildScript.StationID)
	t.Equal(bs.Name, testBuildScript.Name)
	t.DeepEqual(bs.Commands, testBuildScript.Commands)
}

func TestGetBuildScript(tt *testing.T) {
	t := check.T(tt)
	t.Nil(testRepo.truncate())
	addTestData(t)

	bs, err := testRepo.CreateBuildScript(testBuildScript)
	t.Nil(err)

	bsByStation, err := testRepo.GetBuildScriptByStationID(testBuildScript.StationID)
	t.Nil(err)
	t.DeepEqual(bsByStation, bs)

	bsByID, err := testRepo.GetBuildScript(bs.ID)
	t.Nil(err)
	t.DeepEqual(bsByID, bs)

	_, err = testRepo.GetBuildScript(100)
	t.Err(err, app.ErrNotFound)

	_, err = testRepo.GetBuildScriptByStationID(100)
	t.Err(err, app.ErrNotFound)
}

func TestUpdateBuildScript(tt *testing.T) {
	t := check.T(tt)
	t.Nil(testRepo.truncate())
	addTestData(t)

	bs, err := testRepo.CreateBuildScript(testBuildScript)
	t.Nil(err)

	updateBuildScript := app.SetBuildScript{
		StationID: 2,
		Name:      "2",
		Commands:  []string{"4", "5", "6"},
	}
	bs.StationID = updateBuildScript.StationID
	bs.Name = updateBuildScript.Name
	bs.Commands = updateBuildScript.Commands

	bsUpdated, err := testRepo.UpdateBuildScript(bs.ID, updateBuildScript)
	t.Nil(err)
	t.DeepEqual(bsUpdated, bs)

	_, err = testRepo.UpdateBuildScript(100, updateBuildScript)
	t.Err(err, app.ErrNotFound)
}

func TestDeleteBuildScript(tt *testing.T) {
	t := check.T(tt)
	t.Nil(testRepo.truncate())
	addTestData(t)

	bs, err := testRepo.CreateBuildScript(testBuildScript)
	t.Nil(err)

	err = testRepo.DeleteBuildScript(bs.ID)
	t.Nil(err)

	_, err = testRepo.GetBuildScript(bs.ID)
	t.Err(err, app.ErrNotFound)

	err = testRepo.DeleteBuildScript(100)
	t.Err(err, app.ErrNotFound)
}

func TestGetListBuildScript(tt *testing.T) {
	t := check.T(tt)
	t.Nil(testRepo.truncate())
	addTestData(t)

	l, err := testRepo.GetListBuildScripts()
	t.Nil(err)
	t.EQ(len(l), 0)

	bs := testBuildScript

	bs1, err := testRepo.CreateBuildScript(bs)
	t.Nil(err)

	bs.StationID = 2

	bs2, err := testRepo.CreateBuildScript(bs)
	t.Nil(err)

	l, err = testRepo.GetListBuildScripts()
	t.Nil(err)
	t.DeepEqual([]app.BuildScript{bs1, bs2}, l)
}

func TestCreateTask(tt *testing.T) {
	t := check.T(tt)
	t.Nil(testRepo.truncate())
	addTestData(t)

	task, err := testRepo.CreateTask(testCreateTask)
	t.Nil(err)
	t.Equal(task.StationID, testCreateTask.StationID)
	t.Equal(task.VersionID, testCreateTask.VersionID)
	t.Equal(task.Type, task.Type)
	t.Equal(task.Status, app.QueueTaskStatus)
	t.Equal(task.Error, nil)
	t.Equal(task.StartedAt, nil)
	t.Equal(task.StoppedAt, nil)
}

func TestGetTask(tt *testing.T) {
	t := check.T(tt)
	t.Nil(testRepo.truncate())
	addTestData(t)

	task, err := testRepo.CreateTask(testCreateTask)
	t.Nil(err)

	getTask, err := testRepo.GetTask(task.ID)
	t.DeepEqual(getTask, task)

	_, err = testRepo.GetTask(100)
	t.Err(err, app.ErrNotFound)
}

func TestUpdateTask(tt *testing.T) {
	t := check.T(tt)
	t.Nil(testRepo.truncate())
	addTestData(t)

	task, err := testRepo.CreateTask(testCreateTask)
	t.Nil(err)

	status := app.StartedTaskStatus
	errMsg := "error"
	startedAt := time.Now()
	stoppedAt := time.Now()
	updateTask := app.UpdateTask{
		Status:    &status,
		Error:     &errMsg,
		StartedAt: &startedAt,
		StoppedAt: &stoppedAt,
	}

	task.Status = status
	task.Error = &errMsg
	task.StartedAt = &startedAt
	task.StartedAt = &stoppedAt

	updatedTask, err := testRepo.UpdateTask(task.ID, updateTask)
	t.Nil(err)
	t.DeepEqual(updatedTask, task)

	_, err = testRepo.UpdateTask(100, updateTask)
	t.Err(err, app.ErrNotFound)
}

func TestDeleteTask(tt *testing.T) {
	t := check.T(tt)
	t.Nil(testRepo.truncate())
	addTestData(t)

	task, err := testRepo.CreateTask(testCreateTask)
	t.Nil(err)

	err = testRepo.DeleteTask(task.ID)
	t.Nil(err)

	err = testRepo.DeleteTask(task.ID)
	t.Err(err, app.ErrNotFound)

	err = testRepo.DeleteTask(100)
	t.Err(err, app.ErrNotFound)
}

func TestGetListTask(tt *testing.T) {
	t := check.T(tt)
	t.Nil(testRepo.truncate())
	addTestData(t)

	l, err := testRepo.GetListTasks(app.GetListTasksFilter{})
	t.Nil(err)
	t.Equal(len(l), 0)

	createTask := testCreateTask
	task1, err := testRepo.CreateTask(testCreateTask)
	t.Nil(err)

	createTask.StationID = 2
	task2, err := testRepo.CreateTask(testCreateTask)
	t.Nil(err)

	l, err = testRepo.GetListTasks(app.GetListTasksFilter{})
	t.Nil(err)
	t.DeepEqual([]app.Task{task1, task2}, l)

	status := app.QueueTaskStatus
	l, err = testRepo.GetListTasks(app.GetListTasksFilter{Status: &status})
	t.Nil(err)
	t.DeepEqual([]app.Task{task1, task2}, l)

	stationId := app.StationID(1)
	l, err = testRepo.GetListTasks(app.GetListTasksFilter{StationID: &stationId})
	t.Nil(err)
	t.DeepEqual([]app.Task{task1}, l)

	status = app.StartedTaskStatus
	task1, err = testRepo.UpdateTask(task1.ID, app.UpdateTask{Status: &status})
	t.Nil(err)

	l, err = testRepo.GetListTasks(app.GetListTasksFilter{Status: &status})
	t.Nil(err)
	t.DeepEqual([]app.Task{task1}, l)

	l, err = testRepo.GetListTasks(app.GetListTasksFilter{OnlyActive: true})
	t.Nil(err)
	t.DeepEqual([]app.Task{task1}, l)
}
