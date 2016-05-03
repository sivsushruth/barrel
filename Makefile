BASE_DIR = $(shell pwd)
REBAR ?= $(BASE_DIR)/rebar3
OTP_RELEASE=OTP-18.3.2
OTP_VERSION=18.3.2


all: compile

# compilation

compile:
	$(REBAR) compile

# tests

test: eunit

eunit:
	$(REBAR) eunit

# travis

travis_erlang:
	echo "==> build erlang binary for the erlang tests"
	docker build -f Dockerfiles/travis_erlang -t benoitc/travis_erlang .
	docker run -e "OTP_RELEASE=$(OTP_RELEASE)" -e "OTP_VERSION=$(OTP_VERSION)" -i -t -v $(BASE_DIR):/bindir benoitc/travis_erlang
