package app

import (
	"crypto/rand"
	"crypto/rsa"
	"crypto/x509"
	"encoding/json"
	"encoding/pem"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"time"

	"github.com/pkg/sftp"
	"golang.org/x/crypto/ssh"
)

func (a *app) GetPublicKey() (string, error) {
	err := generateSSHKey(a.postControlConfig.KeySSHPath)
	if err != nil {
		return "", err
	}

	key, err := os.ReadFile(a.postControlConfig.KeySSHPath + ".pub")
	if err != nil {
		return "", err
	}

	return string(key), nil
}

func (a *app) GetVersions(stationID StationID) ([]FirmwareVersion, error) {
	a.stationsMutex.RLock()
	defer a.stationsMutex.RUnlock()
	station, ok := a.stations[stationID]
	if !ok || station.LastPing.Add(durationStationOffline).Before(time.Now()) || station.Versions == nil {
		return nil, ErrNotFound
	}

	return station.Versions, nil
}

func (a *app) GetVersionBuffered(stationID StationID) (FirmwareVersion, error) {
	_, err := a.repo.Station(stationID)
	if err != nil {
		return FirmwareVersion{}, err
	}

	file, err := os.Open(path.Join(a.postControlConfig.StationsDirPath, fmt.Sprint(stationID), versionName))
	if err != nil && !os.IsNotExist(err) {
		return FirmwareVersion{}, err
	} else if os.IsNotExist(err) {
		return FirmwareVersion{}, ErrNotFound
	}

	fileBytes, err := io.ReadAll(file)
	if err != nil {
		return FirmwareVersion{}, err
	}

	var version FirmwareVersionJson
	err = json.Unmarshal(fileBytes, &version)
	if err != nil {
		return FirmwareVersion{}, err
	}

	return firmwareVersionFromJson(0, false, version), nil
}

func getVersionsDirNames(client *ssh.Client) ([]string, error) {
	res, err := runRemoteCommand(client, findVersionsOwCommand)
	if err != nil {
		return nil, err
	}
	if len(res) == 0 {
		return []string{}, nil
	}

	return strings.Split(string(res[:len(res)-1]), "\n"), nil
}

func copyFilesToLcw(client *sftp.Client, remotePath, localPath string) error {
	remoteFiles, err := client.ReadDir(remotePath)
	if err != nil {
		return err
	}

	err = os.MkdirAll(localPath, os.ModePerm)
	if err != nil {
		return err
	}

	for _, remoteFile := range remoteFiles {
		remoteFilePath := filepath.Join(remotePath, remoteFile.Name())
		localFilePath := filepath.Join(localPath, remoteFile.Name())

		if remoteFile.IsDir() {
			err := copyFilesToLcw(client, remoteFilePath, localFilePath)
			if err != nil {
				return err
			}
		} else {
			if remoteFile.Name() == paymentWorldName ||
				remoteFile.Name() == paymentWorldConfigName ||
				filepath.Ext(remoteFile.Name()) == ".txt" {
				continue
			}

			srcFile, err := client.Open(remoteFilePath)
			if err != nil {
				return err
			}
			defer srcFile.Close()

			dstFile, err := os.Create(localFilePath)
			if err != nil {
				return err
			}
			defer dstFile.Close()

			_, err = io.Copy(dstFile, srcFile)
			if err != nil {
				return err
			}
		}
	}

	return nil
}

func copyFilesToOw(client *sftp.Client, localPath, remotePath string) error {
	localFiles, err := os.ReadDir(localPath)
	if err != nil {
		return err
	}

	err = client.MkdirAll(remotePath)
	if err != nil {
		return err
	}

	for _, localFile := range localFiles {
		remoteFilePath := filepath.Join(remotePath, localFile.Name())
		localFilePath := filepath.Join(localPath, localFile.Name())

		if localFile.IsDir() {
			err := copyFilesToOw(client, localFilePath, remoteFilePath)
			if err != nil {
				return err
			}
		} else {
			srcFile, err := os.Open(localFilePath)
			if err != nil {
				return err
			}
			defer srcFile.Close()

			dstFile, err := client.Create(remoteFilePath)
			if err != nil {
				return err
			}
			defer dstFile.Close()

			_, err = io.Copy(dstFile, srcFile)
			if err != nil {
				return err
			}

			err = dstFile.Chmod(os.ModePerm)
			if err != nil {
				return err
			}
		}
	}

	return nil
}

func (a *app) GetListBuildScripts() ([]BuildScript, error) {
	buildScripts, err := a.repo.GetListBuildScripts()
	if err != nil {
		return nil, err
	}

	return buildScripts, nil
}

func (a *app) GetBuildScript(id StationID) (BuildScript, error) {
	return a.repo.GetBuildScriptByStationID(id)
}

func (a *app) SetBuildScript(setBuildScript SetBuildScript) (BuildScript, error) {
	_, err := a.repo.Station(setBuildScript.StationID)
	if err != nil {
		return BuildScript{}, err
	}

	if setBuildScript.CopyFromStationID != nil {
		copyFrom, err := a.repo.GetBuildScriptByStationID(*setBuildScript.CopyFromStationID)
		if err != nil {
			return BuildScript{}, err
		}
		setBuildScript.Commands = copyFrom.Commands
	}

	buildScript, err := a.repo.GetBuildScriptByStationID(setBuildScript.StationID)

	if errors.Is(err, ErrNotFound) {
		buildScript, err = a.repo.CreateBuildScript(setBuildScript)
		if err != nil {
			return BuildScript{}, err
		}
		return buildScript, nil
	}
	if err != nil {
		return BuildScript{}, err
	}

	buildScript, err = a.repo.UpdateBuildScript(buildScript.ID, setBuildScript)
	if err != nil {
		return BuildScript{}, err
	}

	return buildScript, nil
}

func (a *app) DeleteBuildScript(id StationID) error {
	return a.repo.DeleteBuildScriptByStationID(id)
}

func (a *app) GetListTasks(filter TasksFilter) (Page[Task], error) {
	tasks, err := a.repo.GetListTasks(filter)
	if err != nil {
		return Page[Task]{}, err
	}

	return tasks, nil
}

func (a *app) GetTask(id int) (Task, error) {
	task, err := a.repo.GetTask(id)
	if err != nil {
		return Task{}, err
	}

	return task, nil
}

func (a *app) DeleteTask(id int) error {
	task, err := a.repo.GetTask(id)
	if err != nil {
		return err
	}

	if task.Status == StartedTaskStatus {
		return ErrTaskStarted
	}

	return a.repo.DeleteTask(id)
}

func (a *app) DeleteTasks() error {
	page := 1
	for {
		tasks, err := a.repo.GetListTasks(TasksFilter{
			Filter: Filter{
				Page:     page,
				PageSize: 100,
			},
			Statuses: []TaskStatus{ErrorTaskStatus, CompletedTaskStatus, CanceledTaskStatus},
		})
		if err != nil {
			return err
		}
		if len(tasks.Items) == 0 {
			break
		}

		for _, v := range tasks.Items {
			err = a.repo.DeleteTask(v.ID)
			if err != nil {
				return err
			}
		}

		if page >= tasks.TotalPages {
			break
		}
		page++
	}

	return nil
}

func (a *app) CreateTask(createTask CreateTask) (Task, error) {
	a.stationsMutex.RLock()
	defer a.stationsMutex.RUnlock()
	station, ok := a.stations[createTask.StationID]
	if !ok || station.LastPing.Add(durationStationOffline).Before(time.Now()) {
		return Task{}, ErrNotFound
	}

	if createTask.VersionID != nil && (createTask.Type != PullFirmwareTaskType && createTask.Type != SetVersionTaskType) ||
		createTask.VersionID == nil && (createTask.Type == PullFirmwareTaskType || createTask.Type == SetVersionTaskType) {
		return Task{}, ErrWrongParameter
	}

	task, err := a.repo.CreateTask(createTask)
	if err != nil {
		return Task{}, err
	}

	return task, nil
}

func (a *app) CopyFirmware(stationID StationID, copyToID StationID) error {
	if stationID == copyToID {
		return nil
	}

	_, err := a.repo.Station(stationID)
	if err != nil {
		return err
	}

	_, err = a.repo.Station(copyToID)
	if err != nil {
		return err
	}

	task, err := a.repo.GetListTasks(TasksFilter{
		Filter: Filter{
			Page:     1,
			PageSize: 1,
		},
		StationsID: []StationID{stationID, copyToID},
		Types:      []TaskType{BuildTaskType, PullFirmwareTaskType},
		Statuses:   []TaskStatus{StartedTaskStatus},
	})
	if err != nil {
		return err
	}

	if len(task.Items) > 0 {
		return ErrTaskStarted
	}

	stationPath := path.Join(a.postControlConfig.StationsDirPath, stationID.String())
	copyToPath := path.Join(a.postControlConfig.StationsDirPath, copyToID.String())

	_, err = os.Stat(stationPath)
	if err != nil && !os.IsNotExist(err) {
		return err
	}
	if os.IsNotExist(err) {
		return ErrStationDirectoryNotExist
	}

	err = os.MkdirAll(copyToPath, os.ModePerm)
	if err != nil {
		return err
	}

	_, err = runLocalComand(fmt.Sprintf(copyFirmwareFromStationToStation, copyToPath, stationPath, copyToPath))
	if err != nil {
		return err
	}

	return nil
}

func sshClient(keyPath, user, ip string) (*ssh.Client, error) {
	privateKeyBytes, err := os.ReadFile(keyPath)
	if err != nil {
		return nil, err
	}

	privateKey, err := ssh.ParsePrivateKey(privateKeyBytes)
	if err != nil {
		return nil, err
	}

	config := &ssh.ClientConfig{
		User: user,
		Auth: []ssh.AuthMethod{
			ssh.PublicKeys(privateKey),
		},
		HostKeyCallback: ssh.InsecureIgnoreHostKey(),
	}

	client, err := ssh.Dial("tcp", ip+":22", config)
	if err != nil {
		return nil, err
	}
	return client, nil
}

func runRemoteCommand(client *ssh.Client, command string) ([]byte, error) {
	session, err := client.NewSession()
	if err != nil {
		return nil, err
	}
	defer session.Close()

	return session.CombinedOutput(command)
}

func runLocalComand(command string) ([]byte, error) {
	cmd := exec.Command("bash", "-c", command)
	return cmd.CombinedOutput()
}

func generateSSHKey(keyPath string) error {
	_, err := os.Stat(keyPath + ".pub")
	if err != nil && !os.IsNotExist(err) {
		return err
	}
	if !os.IsNotExist(err) {
		return nil
	}

	privateKey, err := rsa.GenerateKey(rand.Reader, 2048)
	if err != nil {
		return err
	}

	sshDir := filepath.Dir(keyPath)
	if err := os.MkdirAll(sshDir, os.ModePerm); err != nil {
		return err
	}

	pubKey, err := ssh.NewPublicKey(&privateKey.PublicKey)
	if err != nil {
		return err
	}

	if err := os.WriteFile(keyPath+".pub", ssh.MarshalAuthorizedKey(pubKey), 0700); err != nil {
		return err
	}

	privateKeyPEM := &pem.Block{
		Type:  "RSA PRIVATE KEY",
		Bytes: x509.MarshalPKCS1PrivateKey(privateKey),
	}

	if err := os.WriteFile(keyPath, pem.EncodeToMemory(privateKeyPEM), 0700); err != nil {
		return err
	}

	return nil
}

func (a *app) taskScheduler() {
	for {
		time.Sleep(time.Second * 5)

		sort := CreatedAtAscTaskSort
		allTasks, err := a.repo.GetListTasks(TasksFilter{
			Statuses: []TaskStatus{QueueTaskStatus, StartedTaskStatus},
			Sort:     &sort,
		})
		if err != nil {
			log.PrintErr(err)
			continue
		}
		if len(allTasks.Items) == 0 {
			continue
		}

		a.stationsMutex.Lock()
		for i, station := range a.stations {
			if station.Task != nil || station.LastPing.Add(durationStationOffline).Before(time.Now()) {
				continue
			}

			stationTask, err := a.repo.GetListTasks(TasksFilter{
				Filter: Filter{
					Page:     1,
					PageSize: 1,
				},
				StationsID: []StationID{station.ID},
				Statuses:   []TaskStatus{QueueTaskStatus, StartedTaskStatus},
				Sort:       &sort,
			})
			if err != nil {
				log.PrintErr(err)
				continue
			}
			if len(stationTask.Items) == 0 {
				continue
			}

			task := stationTask.Items[0]
			station.Task = &task
			a.stations[i] = station

			go a.runTask(task)
		}
		a.stationsMutex.Unlock()
	}
}

func (a *app) runTask(task Task) {
	switch task.Type {
	case BuildTaskType:
		a.runBuild(task)
	case UpdateTaskType:
		a.runUpdate(task)
	case RebootTaskType:
		a.runReboot(task)
	case GetVersionsTaskType:
		a.runGetVersions(task)
	case PullFirmwareTaskType:
		a.runPullFirmware(task)
	case SetVersionTaskType:
		a.runSetVersion(task)
	default:
		panic("Unknown task type: " + task.Type)
	}
}

func (a *app) runBuild(task Task) {
	task, ip, err := a.prepareTask(task)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error preparing the task: %s", err.Error()))
		return
	}

	script, err := a.repo.GetBuildScriptByStationID(task.StationID)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error getting build script for this station: %s", err.Error()))
		return
	}

	stationPath := path.Join(a.postControlConfig.StationsDirPath, task.StationID.String())
	err = os.MkdirAll(stationPath, os.ModePerm)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error creating station directory: %s", err.Error()))
		return
	}

	_, err = runLocalComand(firmwareTempCreate)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error creating firmware temp directory: %s", err.Error()))
		return
	}

	client, err := sshClient(a.postControlConfig.KeySSHPath, a.postControlConfig.UserSSH, ip)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error creating ssh client: %s", err.Error()))
		return
	}
	defer client.Close()

	sftpClient, err := sftp.NewClient(client)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error creating sftp client: %s", err.Error()))
		return
	}
	defer sftpClient.Close()

	homeOwPath, err := sftpClient.Getwd()
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error geting home path: %s", err.Error()))
		return
	}

	_, err = sftpClient.Stat(path.Join(homeOwPath, "openwashing"))
	if err != nil && !os.IsNotExist(err) {
		a.handleTaskErr(task, fmt.Sprintf("Error checking directory \"~/openwashing\": %s", err.Error()))
		return
	}

	if os.IsNotExist(err) {
		_, err = runRemoteCommand(client, cloneRepositoryOwCommand)
		if err != nil {
			a.handleTaskErr(task, fmt.Sprintf("Error cloning repository: %s", err.Error()))
			return
		}
	} else {
		_, err = runRemoteCommand(client, pullRepositoryOwCommand)
		if err != nil {
			a.handleTaskErr(task, fmt.Sprintf("Error pulling repository: %s", err.Error()))
			return
		}
	}

	_, err = runRemoteCommand(client, makeBinarOwCommand)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error making: %s", err.Error()))
		return
	}

	binarFile, err := sftpClient.Open(path.Join(homeOwPath, binarOwPath))
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error opening binar file: %s", err.Error()))
		return
	}
	defer binarFile.Close()

	binarBytes, err := io.ReadAll(binarFile)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error read binar file: %s", err.Error()))
		return
	}

	binarFileForSave, err := os.Create(path.Join(stationPath, binarName))
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error creating file %s: %s", binarName, err.Error()))
		return
	}
	defer binarFileForSave.Close()

	_, err = binarFileForSave.Write(binarBytes)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error writing binar: %s", err.Error()))
		return
	}

	homeLwcPath, err := os.UserHomeDir()
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error geting home path: %s", err.Error()))
		return
	}

	_, err = os.Stat(path.Join(homeLwcPath, openwashingLcwName))
	if err != nil && !os.IsNotExist(err) {
		a.handleTaskErr(task, fmt.Sprintf("Error checking directory \"~/openwashing\": %s", err.Error()))
		return
	}

	if os.IsNotExist(err) {
		_, err = runLocalComand(cloneRepositoryLcwCommand)
		if err != nil {
			a.handleTaskErr(task, fmt.Sprintf("Error cloning repository: %s", err.Error()))
			return
		}
	} else {
		_, err = runLocalComand(pullRepositoryLcwCommand)
		if err != nil {
			a.handleTaskErr(task, fmt.Sprintf("Error pulling repository: %s", err.Error()))
			return
		}
	}

	for _, command := range script.Commands {
		log.Println(command)
		_, err = runLocalComand(command)
		if err != nil {
			a.handleTaskErr(task, fmt.Sprintf("Error executing build script (%s): %s", command, err.Error()))
			return
		}
	}

	hashBinar, err := runLocalComand(fmt.Sprintf(hashBinarLcwCommand, stationPath))
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error calculating binary file hash: %s", err.Error()))
		return
	}
	if len(hashBinar) == 0 {
		a.handleTaskErr(task, "Error calculating binary file hash: hash is null")
		return
	}
	hashBinar = hashBinar[:len(hashBinar)-1]

	hashLua, err := runLocalComand(hashLuaLcwCommand)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error calculating lua script file hash: %s", err.Error()))
		return
	}
	if len(hashLua) == 0 {
		a.handleTaskErr(task, "Error calculating lua script file hash: hash is null")
		return
	}
	hashLua = hashLua[:len(hashLua)-1]

	hashEnv, err := runLocalComand(hashEnvLcwCommand)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error calculating env hash: %s", err.Error()))
		return
	}
	if len(hashEnv) == 0 {
		a.handleTaskErr(task, "Error calculating env hash: hash is null")
		return
	}
	hashEnv = hashEnv[:len(hashEnv)-1]

	commitedAt, err := runLocalComand(commitedAtLcwCommand)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error calculating commited date: %s", err.Error()))
		return
	}
	if len(commitedAt) == 0 {
		a.handleTaskErr(task, "Error calculating commited date: date is null")
		return
	}
	commitedAtDate, err := time.Parse("2006-01-02T15:04:05Z", string(commitedAt[:len(commitedAt)-1]))
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error calculating commited date: %s", err.Error()))
		return
	}

	firmwareVersion, err := json.MarshalIndent(FirmwareVersionJson{
		HashLua:    string(hashLua),
		HashEnv:    string(hashEnv),
		HashBinar:  string(hashBinar),
		BuiltAt:    time.Now(),
		CommitedAt: commitedAtDate,
	}, "", "\t")
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error marshaling firmware version: %s", err.Error()))
		return
	}

	file, err := os.Create(path.Join(stationPath, versionName))
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error creating file %s: %s", versionName, err.Error()))
		return
	}
	defer file.Close()

	_, err = file.Write(firmwareVersion)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error writing file %s: %s", binarName, err.Error()))
		return
	}

	_, err = runLocalComand(fmt.Sprintf(cpLcwCommand, stationPath, stationPath))
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error copying: %s", err.Error()))
		return
	}

	a.compliteTask(task)
}

func (a *app) runGetVersions(task Task) {
	task, ip, err := a.prepareTask(task)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error preparing the task: %s", err.Error()))
		return
	}

	client, err := sshClient(a.postControlConfig.KeySSHPath, a.postControlConfig.UserSSH, ip)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error creating ssh client: %s", err.Error()))
		return
	}
	defer client.Close()

	sftpClient, err := sftp.NewClient(client)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error creating sftp client: %s", err.Error()))
		return
	}
	defer sftpClient.Close()

	homeOwPath, err := sftpClient.Getwd()
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error geting home path: %s", err.Error()))
		return
	}

	currentVersion := 0
	linkPath := path.Join(homeOwPath, currentWashName)
	currentWashPath, err := sftpClient.ReadLink(linkPath)
	if err != nil && !os.IsNotExist(err) {
		a.handleTaskErr(task, fmt.Sprintf("Error geting current wash dir name: %s", err.Error()))
		return
	}
	if !os.IsNotExist(err) {
		linkName := path.Base(currentWashPath)
		if linkName != baseWashName {
			if len(linkName) < 5 {
				a.handleTaskErr(task, fmt.Sprintf("Error: %s has a length less than 5, so a version cannot be derived from it", linkName))
				return
			}

			currentVersion, err = strconv.Atoi(linkName[5:])
			if err != nil {
				a.handleTaskErr(task, fmt.Sprintf("Error parsing current version: %s", err.Error()))
				return
			}
		}
	}

	directories, err := getVersionsDirNames(client)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error geting versions: %s", err.Error()))
		return
	}

	versions := make([]FirmwareVersion, 0, len(directories))
	var currentVersions *FirmwareVersion = nil

	for _, dir := range directories {
		dirName := path.Base(dir)
		if dirName == baseWashName {
			versionFromJson := FirmwareVersion{
				IsCurrent: currentVersion == 0,
			}
			versions = append(versions, versionFromJson)
			if versionFromJson.IsCurrent {
				currentVersions = &versionFromJson
			}
			continue
		}

		if len(dirName) < 5 {
			a.handleTaskErr(task, fmt.Sprintf("Error: %s has a length less than 5, so a version cannot be derived from it", dirName))
			return
		}

		v, err := strconv.Atoi(dirName[5:])
		if err != nil {
			a.handleTaskErr(task, fmt.Sprintf("Error parsing version: %s", err.Error()))
			return
		}

		versionFilePath := path.Join(dir, versionName)
		file, err := sftpClient.Open(versionFilePath)
		if err != nil {
			a.handleTaskErr(task, fmt.Sprintf("Error opening file %s: %s", versionFilePath, err.Error()))
			return
		}
		defer file.Close()

		text, err := io.ReadAll(file)
		if err != nil {
			a.handleTaskErr(task, fmt.Sprintf("Error reading version file: %s", err.Error()))
			return
		}

		var version FirmwareVersionJson
		err = json.Unmarshal(text, &version)
		if err != nil {
			a.handleTaskErr(task, fmt.Sprintf("Error parsing version file to json: %s", err.Error()))
			return
		}

		versionFromJson := firmwareVersionFromJson(v, v == currentVersion, version)
		versions = append(versions, versionFromJson)

		if versionFromJson.IsCurrent {
			currentVersions = &versionFromJson
		}
	}

	sort.Slice(versions, func(i, j int) bool {
		return versions[i].ID > versions[j].ID
	})

	a.stationsMutex.Lock()
	station, ok := a.stations[task.StationID]
	if !ok {
		a.stationsMutex.RUnlock()
		a.handleTaskErr(task, "Error: station not found")
		return
	}
	station.Versions = versions
	station.CurrentVersions = currentVersions
	a.stations[task.StationID] = station
	a.stationsMutex.Unlock()

	a.compliteTask(task)
}

func (a *app) runPullFirmware(task Task) {
	task, ip, err := a.prepareTask(task)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error preparing the task: %s", err.Error()))
		return
	}

	if task.VersionID == nil {
		a.handleTaskErr(task, "Error: firmware version not specified")
		return
	}

	client, err := sshClient(a.postControlConfig.KeySSHPath, a.postControlConfig.UserSSH, ip)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error creating ssh client: %s", err.Error()))
		return
	}
	defer client.Close()

	sftpClient, err := sftp.NewClient(client)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error creating sftp client: %s", err.Error()))
		return
	}
	defer sftpClient.Close()

	homeOwPath, err := sftpClient.Getwd()
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error geting home path: %s", err.Error()))
		return
	}

	remotePath := path.Join(homeOwPath, fmt.Sprintf("wash_%d", *task.VersionID))
	if *task.VersionID == 0 {
		remotePath = path.Join(homeOwPath, baseWashName)
	}

	_, err = sftpClient.Stat(remotePath)
	if err != nil && !os.IsNotExist(err) {
		a.handleTaskErr(task, fmt.Sprintf("Error checking file existence %s: %s", remotePath, err.Error()))
		return
	}
	if os.IsNotExist(err) {
		a.handleTaskErr(task, "Error: the specified firmware version does not exist")
		return
	}

	stationPath := path.Join(a.postControlConfig.StationsDirPath, task.StationID.String())
	err = os.RemoveAll(stationPath)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error deleting directory %s: %s", stationPath, err.Error()))
		return
	}

	err = copyFilesToLcw(sftpClient, remotePath, stationPath)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error copying remote files: %s", err.Error()))
		return
	}

	if *task.VersionID == 0 {
		versionsFile, err := os.Create(path.Join(stationPath, versionName))
		if err != nil {
			a.handleTaskErr(task, fmt.Sprintf("Error creating file %s: %s", versionName, err.Error()))
			return
		}
		defer versionsFile.Close()

		bytes, err := json.MarshalIndent(FirmwareVersionJson{}, "", "\t")
		if err != nil {
			a.handleTaskErr(task, fmt.Sprintf("Error marshaling firmware version: %s", err.Error()))
			return
		}

		_, err = versionsFile.Write(bytes)
		if err != nil {
			a.handleTaskErr(task, fmt.Sprintf("Error writing file %s: %s", versionName, err.Error()))
			return
		}
	}

	pwPath := path.Join(a.postControlConfig.StationsDirPath, paymentWorldName)
	pwcPath := path.Join(a.postControlConfig.StationsDirPath, paymentWorldConfigName)

	_, errPw := os.Stat(pwPath)
	if errPw != nil && !os.IsNotExist(errPw) {
		a.handleTaskErr(task, fmt.Sprintf("Error checking file existence %s: %s", pwPath, errPw.Error()))
		return
	}

	_, errPwc := os.Stat(pwcPath)
	if errPwc != nil && !os.IsNotExist(errPwc) {
		a.handleTaskErr(task, fmt.Sprintf("Error checking file existence %s: %s", pwcPath, errPwc.Error()))
		return
	}

	log.Printf("Банк1: %s, %s", errPw, errPwc)

	if os.IsNotExist(errPw) || os.IsNotExist(errPwc) {
		pwRemotePath := path.Join(remotePath, paymentWorldName)
		pwcRemotePath := path.Join(remotePath, paymentWorldConfigName)

		pwRemoteFile, errPw := sftpClient.Open(pwRemotePath)
		if errPw != nil && !os.IsNotExist(errPw) {
			a.handleTaskErr(task, fmt.Sprintf("Error opening file %s: %s", pwRemotePath, errPw.Error()))
			return
		}

		pwcRemoteFile, errPwc := sftpClient.Open(pwcRemotePath)
		if errPwc != nil && !os.IsNotExist(errPwc) {
			a.handleTaskErr(task, fmt.Sprintf("Error opening file %s: %s", pwcRemotePath, errPwc.Error()))
			return
		}

		log.Printf("Банк2: %s, %s", errPw, errPwc)

		if errPw == nil && errPwc == nil {
			defer pwRemoteFile.Close()
			defer pwcRemoteFile.Close()

			pwFile, err := os.Create(pwPath)
			if err != nil {
				a.handleTaskErr(task, fmt.Sprintf("Error creating file %s: %s", pwPath, err.Error()))
				return
			}
			defer pwFile.Close()

			_, err = io.Copy(pwFile, pwRemoteFile)
			if err != nil {
				a.handleTaskErr(task, fmt.Sprintf("Error coping file %s to %s: %s", pwRemotePath, pwPath, err.Error()))
				return
			}

			pwcFile, err := os.Create(pwcPath)
			if err != nil {
				a.handleTaskErr(task, fmt.Sprintf("Error creating file %s: %s", pwcPath, err.Error()))
				return
			}
			defer pwcFile.Close()

			_, err = io.Copy(pwcFile, pwcRemoteFile)
			if err != nil {
				a.handleTaskErr(task, fmt.Sprintf("Error coping file %s to %s: %s", pwcRemotePath, pwcPath, err.Error()))
				return
			}
		}
	}

	a.compliteTask(task)
}

func (a *app) runUpdate(task Task) {
	task, ip, err := a.prepareTask(task)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error preparing the task: %s", err.Error()))
		return
	}

	client, err := sshClient(a.postControlConfig.KeySSHPath, a.postControlConfig.UserSSH, ip)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error creating ssh client: %s", err.Error()))
		return
	}
	defer client.Close()

	sftpClient, err := sftp.NewClient(client)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error creating sftp client: %s", err.Error()))
		return
	}
	defer sftpClient.Close()

	stationPath := path.Join(a.postControlConfig.StationsDirPath, task.StationID.String())
	_, err = os.Stat(stationPath)
	if err != nil && !os.IsNotExist(err) {
		a.handleTaskErr(task, fmt.Sprintf("Error checking directory \"%s\": %s", stationPath, err.Error()))
		return
	}
	if os.IsNotExist(err) {
		a.handleTaskErr(task, fmt.Sprintf("Error: directory \"%s\" does not exist", stationPath))
		return
	}

	versionDirs, err := getVersionsDirNames(client)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error geting versions: %s", err.Error()))
		return
	}

	newVersion := 0
	for _, dir := range versionDirs {
		dirName := path.Base(dir)
		if dirName == baseWashName {
			continue
		}

		if len(dirName) < 5 {
			a.handleTaskErr(task, fmt.Sprintf("Error: %s has a length less than 5, so a version cannot be derived from it", dirName))
			return
		}

		v, err := strconv.Atoi(dirName[5:])
		if err != nil {
			a.handleTaskErr(task, fmt.Sprintf("Error parsing version: %s", err.Error()))
			return
		}
		if v > newVersion {
			newVersion = v
		}
	}
	newVersion++

	homeOwPath, err := sftpClient.Getwd()
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error geting home path: %s", err.Error()))
		return
	}

	washDir := path.Join(homeOwPath, fmt.Sprintf("wash_%d", newVersion))
	err = copyFilesToOw(sftpClient, stationPath, washDir)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error copying files to the station: %s", err.Error()))
		return
	}

	pwPath := path.Join(a.postControlConfig.StationsDirPath, paymentWorldName)
	pwFile, err := os.Open(pwPath)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error opening file %s: %s", pwPath, err.Error()))
		return
	}
	defer pwFile.Close()

	pwRemotePath := path.Join(washDir, paymentWorldName)
	pwRemoteFile, err := sftpClient.Create(pwRemotePath)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error creating file %s: %s", pwRemotePath, err.Error()))
		return
	}
	defer pwRemoteFile.Close()

	_, err = io.Copy(pwRemoteFile, pwFile)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error coping file %s to %s: %s", pwRemotePath, pwPath, err.Error()))
		return
	}

	pwcPath := path.Join(a.postControlConfig.StationsDirPath, paymentWorldConfigName)
	pwcFile, err := os.Open(pwcPath)
	if err != nil && !os.IsNotExist(err) {
		a.handleTaskErr(task, fmt.Sprintf("Error opening file %s: %s", pwcPath, err.Error()))
		return
	}
	defer pwcFile.Close()

	pwcRemotePath := path.Join(washDir, paymentWorldConfigName)
	pwcRemoteFile, err := sftpClient.Create(pwcRemotePath)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error creating file %s: %s", pwcRemotePath, err.Error()))
		return
	}
	defer pwcRemoteFile.Close()

	_, err = io.Copy(pwcRemoteFile, pwcFile)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error coping file %s to %s: %s", pwcRemotePath, pwcPath, err.Error()))
		return
	}

	linkPath := path.Join(homeOwPath, currentWashName)
	_, err = runRemoteCommand(client, fmt.Sprintf(cleateLink, washDir, linkPath))
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error creating link: %s", err.Error()))
		return
	}

	runsh, err := sftpClient.Open(path.Join(homeOwPath, runshName))
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error opening file run.sh: %s", err.Error()))
		return
	}

	text, err := io.ReadAll(runsh)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error reading file run.sh: %s", err.Error()))
		return
	}

	err = runsh.Close()
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error closing file run.sh: %s", err.Error()))
		return
	}

	lines := strings.Split(string(text), "\n")
	if len(lines) < 2 {
		a.handleTaskErr(task, "Error: file run.sh contains less than two lines")
		return
	}

	cdCommand := fmt.Sprintf(cdFirmwareRunshCommand, linkPath)
	if lines[1] != cdCommand {

		lines[1] = cdCommand
		text = []byte(strings.Join(lines, "\n"))

		runsh, err = sftpClient.Create(path.Join(homeOwPath, runshName))
		if err != nil {
			a.handleTaskErr(task, fmt.Sprintf("Error opening file run.sh: %s", err.Error()))
			return
		}
		defer runsh.Close()

		_, err = runsh.Write(text)
		if err != nil {
			a.handleTaskErr(task, fmt.Sprintf("Error writing file run.sh: %s", err.Error()))
			return
		}
	}

	a.compliteTask(task)
}

func (a *app) runReboot(task Task) {
	task, ip, err := a.prepareTask(task)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error preparing the task: %s", err.Error()))
		return
	}

	client, err := sshClient(a.postControlConfig.KeySSHPath, a.postControlConfig.UserSSH, ip)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error creating ssh client: %s", err.Error()))
		return
	}
	defer client.Close()

	res, err := runRemoteCommand(client, rebootOwCommand)
	if err != nil {
		log.PrintErr(string(res))
		a.handleTaskErr(task, fmt.Sprintf("Error executing reboot command: %s", err.Error()))
		return
	}

	a.compliteTask(task)
}

func (a *app) runSetVersion(task Task) {
	task, ip, err := a.prepareTask(task)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error preparing the task: %s", err.Error()))
		return
	}

	if task.VersionID == nil {
		a.handleTaskErr(task, "Error: firmware version not specified")
		return
	}

	client, err := sshClient(a.postControlConfig.KeySSHPath, a.postControlConfig.UserSSH, ip)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error creating ssh client: %s", err.Error()))
		return
	}
	defer client.Close()

	sftpClient, err := sftp.NewClient(client)
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error creating sftp client: %s", err.Error()))
		return
	}
	defer sftpClient.Close()

	homeOwPath, err := sftpClient.Getwd()
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error geting home path: %s", err.Error()))
		return
	}

	remotePath := path.Join(homeOwPath, fmt.Sprintf("wash_%d", *task.VersionID))
	if *task.VersionID == 0 {
		remotePath = path.Join(homeOwPath, baseWashName)
	}

	_, err = sftpClient.Stat(remotePath)
	if err != nil && !os.IsNotExist(err) {
		a.handleTaskErr(task, fmt.Sprintf("Error checking file existence %s: %s", remotePath, err.Error()))
		return
	}
	if os.IsNotExist(err) {
		a.handleTaskErr(task, "Error: the specified firmware version does not exist")
		return
	}

	_, err = runRemoteCommand(client, fmt.Sprintf(cleateLink, remotePath, path.Join(homeOwPath, currentWashName)))
	if err != nil {
		a.handleTaskErr(task, fmt.Sprintf("Error creating link: %s", err.Error()))
		return
	}

	a.compliteTask(task)
}

func (a *app) prepareTask(task Task) (Task, string, error) {
	status := StartedTaskStatus
	startedAt := time.Now()
	task, err := a.repo.UpdateTask(task.ID, UpdateTask{
		Status:    &status,
		StartedAt: &startedAt,
	})
	if err != nil {
		return Task{}, "", err
	}

	a.stationsMutex.RLock()
	defer a.stationsMutex.RUnlock()
	station, ok := a.stations[task.StationID]
	if !ok {
		return Task{}, "", ErrNotFound
	}
	ip := station.IP

	return task, ip, nil
}

func (a *app) compliteTask(task Task) {
	status := CompletedTaskStatus
	stoppedAt := time.Now()
	task, err := a.repo.UpdateTask(task.ID, UpdateTask{
		Status:    &status,
		StoppedAt: &stoppedAt,
	})
	if err != nil {
		log.PrintErr(err)
	}

	a.stationsMutex.Lock()
	defer a.stationsMutex.Unlock()
	station, ok := a.stations[task.StationID]
	if !ok {
		log.PrintErr("Station not found: %d", task.StationID)
	}
	station.Task = nil
	a.stations[task.StationID] = station
}

func (a *app) handleTaskErr(task Task, msg string) {
	log.PrintErr(msg)

	status := ErrorTaskStatus
	stoppedAt := time.Now()
	task, err := a.repo.UpdateTask(task.ID, UpdateTask{
		Status:    &status,
		Error:     &msg,
		StoppedAt: &stoppedAt,
	})
	if err != nil {
		log.PrintErr(err)
	}

	page := 1
	for {
		queueTasks, err := a.repo.GetListTasks(TasksFilter{
			Filter: Filter{
				Page:     page,
				PageSize: 100,
			},
			StationsID: []StationID{task.StationID},
			Statuses:   []TaskStatus{QueueTaskStatus},
		})
		if err != nil {
			log.PrintErr(err)
			return
		}

		status = CanceledTaskStatus
		errorMsg := fmt.Sprintf("The task was canceled due to an error in the task %d", task.ID)
		for _, queueTask := range queueTasks.Items {
			_, err := a.repo.UpdateTask(queueTask.ID, UpdateTask{
				Status:    &status,
				StoppedAt: &stoppedAt,
				Error:     &errorMsg,
			})
			if err != nil {
				log.PrintErr(err)
			}
		}

		if page >= queueTasks.TotalPages {
			break
		}
		page++
	}

	a.stationsMutex.Lock()
	defer a.stationsMutex.Unlock()
	station, ok := a.stations[task.StationID]
	if !ok {
		log.PrintErr("Station not found: %d", task.StationID)
		return
	}
	station.Task = nil
	a.stations[task.StationID] = station
}
