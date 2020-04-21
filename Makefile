build:
	erl -make

run:
	erl -pa ebin/ -eval "application:start(erlsmtp)"

build-run: build run