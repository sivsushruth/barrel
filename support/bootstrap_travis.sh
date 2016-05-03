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
mkdir -p ~/otp && tar -xvf erlang-18.3.2-nonroot.tar.bz2 -C ~/otp/
mkdir -p ~/.kerl
source $HOME/otp/18.3.2/activate

ERL_VSN=`erl -eval '{ok, Version} = file:read_file(filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"])), erlang:display(erlang:binary_to_list(Version)), halt().' -noshell`
echo "Installed version of Erlang: $ERL_VSN"


./rebar3 update
