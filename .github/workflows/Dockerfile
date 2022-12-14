ARG BASE_IMAGE
ARG K_VERSION
ARG Z3_VERSION
FROM z3:${Z3_VERSION} as Z3

ARG BASE_IMAGE
ARG K_VERSION
FROM runtimeverificationinc/kframework-k:ubuntu-${BASE_IMAGE}-${K_VERSION}

COPY --from=Z3 /usr/bin/z3 /usr/bin/z3

RUN    apt-get update            \
    && apt-get upgrade --yes     \
    && apt-get install --yes     \
            cmake                \
            curl                 \
            libcrypto++-dev      \
            libprocps-dev        \
            libsecp256k1-dev     \
            libssl-dev           \
            libyaml-dev          \
            pkg-config           \
            python3              \
            python3-pip          \
            zlib1g-dev           \
            xxd

RUN pip3 install virtualenv

RUN curl -sSL https://get.haskellstack.org/ | sh

RUN curl -sSL https://install.python-poetry.org | POETRY_HOME=/usr python3 - && poetry --version

ARG USER=user
ARG GROUP=$USER
ARG USER_ID=1000
ARG GROUP_ID=$USER_ID
RUN groupadd -g $GROUP_ID $GROUP && useradd -m -u $USER_ID -s /bin/sh -g $GROUP $USER

USER $USER:$GROUP
