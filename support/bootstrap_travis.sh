#!/usr/bin/env bash

export KERL_CONFIGURE_OPTIONS="--disable-hipe --enable-smp-support \
    --enable-threads --enable-kernel-poll --without-odbc \
    --enable-dirty-schedulers"

curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl
chmod +x kerl

OTP_RELEASE="OTP-$TRAVIS_OTP_RELEASE"

if [ "$TRAVIS_OTP_RELEASE" = "18.3" ]; then
    OTP_RELEASE="18.3.2"
fi

wget https://barrel-db.org/dl/erlang-18.3.2-nonroot.tar.bz2
mkdir -p ~/otp && tar -xf erlang-18.3.2-nonroot.tar.bz2 -C ~/otp/
mkdir -p ~/.kerl
source $HOME/otp/18.3.2/activate

./rebar3 update
