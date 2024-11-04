build:
	stack build
	npm run tailwind:prod

deploy:
	stack install --local-bin-path bin
	sudo systemctl kill budget
	cp bin/budget-exe ../webapps/budget
	cp output.css ../webapps/public/output.css
	sudo systemctl start budget

backup:
	rclone copy "/budget-database/budget.db" "gdrive:/Budget Database Backup"