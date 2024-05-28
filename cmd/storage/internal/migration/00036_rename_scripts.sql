-- +goose Up
-- SQL in this section is executed when the migration is applied.

UPDATE build_scripts
SET name = 'Вода/активная пена/воск/воздух/пылесос/пауза', commands = '["cd ./samples/wash \u0026\u0026 cp -r ./script.lua ./main.json ./constants.lua ./btn_animation ./screens ./pic ../../firmware","","mkdir ./firmware/programs","","cp -r ./samples/wash/all_programs/water/. ./firmware/programs/p1","","cp -r ./samples/wash/all_programs/foam_active/. ./firmware/programs/p2","","cp -r ./samples/wash/all_programs/wax/. ./firmware/programs/p3","","cp -r ./samples/wash/all_programs/air/. ./firmware/programs/p4","","cp -r ./samples/wash/all_programs/vacuum/. ./firmware/programs/p5","","cp -r ./samples/wash/all_programs/pause/. ./firmware/programs/p6"]'
WHERE name = 'Прошивка 1';

UPDATE build_scripts
SET name = 'Вода/пена/активная пена/воск/осмос/пауза', commands = '["cd ./samples/wash \u0026\u0026 cp -r ./script.lua ./main.json ./constants.lua ./btn_animation ./screens ./pic ../../firmware","","mkdir ./firmware/programs","","cp -r ./samples/wash/all_programs/water/. ./firmware/programs/p1","","cp -r ./samples/wash/all_programs/foam/. ./firmware/programs/p2","","cp -r ./samples/wash/all_programs/foam_active/. ./firmware/programs/p3","","cp -r ./samples/wash/all_programs/wax/. ./firmware/programs/p4","","cp -r ./samples/wash/all_programs/osmosian/. ./firmware/programs/p5","","cp -r ./samples/wash/all_programs/pause/. ./firmware/programs/p6"]'
WHERE name = 'Прошивка 2';

-- +goose Down
-- SQL in this section is executed when the migration is rolled back.
