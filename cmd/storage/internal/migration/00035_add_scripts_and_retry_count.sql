-- +goose Up
-- SQL in this section is executed when the migration is applied.

ALTER TABLE tasks ADD COLUMN retry_count INT DEFAULT 0;

DELETE FROM build_scripts;

INSERT INTO build_scripts (station_id, name, commands)  
SELECT id, 'Прошивка 1', '["cd ~/samples/wash \u0026\u0026 cp -r ./script.lua ./main.json ./constants.lua ./btn_animation ./screens ./pic ./programs ~/firmware_temp","","rm -rf ~/firmware_temp/programs/p1 \u0026\u0026 cp -r ~/samples/wash/all_programs/water/. ~/firmware_temp/programs/p1","","rm -rf ~/firmware_temp/programs/p2 \u0026\u0026 cp -r ~/samples/wash/all_programs/foam_active/. ~/firmware_temp/programs/p2","","rm -rf ~/firmware_temp/programs/p3 \u0026\u0026 cp -r ~/samples/wash/all_programs/wax/. ~/firmware_temp/programs/p3","","rm -rf ~/firmware_temp/programs/p4 \u0026\u0026 cp -r ~/samples/wash/all_programs/air/. ~/firmware_temp/programs/p4","","rm -rf ~/firmware_temp/programs/p5 \u0026\u0026 cp -r ~/samples/wash/all_programs/vacuum/. ~/firmware_temp/programs/p5","","rm -rf ~/firmware_temp/programs/p6 \u0026\u0026 cp -r ~/samples/wash/all_programs/pause/. ~/firmware_temp/programs/p6"]'
FROM station
WHERE id <= 6;

INSERT INTO build_scripts (station_id, name, commands)  
SELECT id, 'Прошивка 2', '["cd ~/samples/wash \u0026\u0026 cp -r ./script.lua ./main.json ./constants.lua ./btn_animation ./screens ./pic ./programs ~/firmware_temp","","rm -rf ~/firmware_temp/programs/p1 \u0026\u0026 cp -r ~/samples/wash/all_programs/water/. ~/firmware_temp/programs/p1","","rm -rf ~/firmware_temp/programs/p2 \u0026\u0026 cp -r ~/samples/wash/all_programs/foam/. ~/firmware_temp/programs/p2","","rm -rf ~/firmware_temp/programs/p3 \u0026\u0026 cp -r ~/samples/wash/all_programs/foam_active/. ~/firmware_temp/programs/p3","","rm -rf ~/firmware_temp/programs/p4 \u0026\u0026 cp -r ~/samples/wash/all_programs/wax/. ~/firmware_temp/programs/p4","","rm -rf ~/firmware_temp/programs/p5 \u0026\u0026 cp -r ~/samples/wash/all_programs/osmosian/. ~/firmware_temp/programs/p5","","rm -rf ~/firmware_temp/programs/p6 \u0026\u0026 cp -r ~/samples/wash/all_programs/pause/. ~/firmware_temp/programs/p6"]'
FROM station
WHERE id > 6;

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.

ALTER TABLE tasks DROP COLUMN retry_count;
