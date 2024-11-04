build:
	stack build

install:
	sudo chmod a+x deployment/setting_up_systemd.sh
	./deployment/setting_up_systemd.sh

deploy:
	stack install --local-bin-path bin
	sudo cp bin/budget-exe /usr/local/bin/budget/budget
	sudo systemctl kill budget
	sudo systemctl start budget

backup:
	rclone copy "/budget-database/budget.db" "gdrive:/Budget Database Backup"