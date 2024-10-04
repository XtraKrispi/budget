build:
	stack install --local-bin-path ./bin

deploy:
	sudo cp -r -f ../build/erlang-shipment/. /home/xtrakrispi/webapps/budget
	sudo systemctl kill budget
	sudo systemctl start budget