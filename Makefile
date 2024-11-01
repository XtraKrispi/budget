build:
	stack build

deploy:
	sudo stack install --local-bin-path /usr/local/bin
	sudo systemctl kill budget
	sudo systemctl start budget