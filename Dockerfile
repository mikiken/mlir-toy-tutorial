FROM ubuntu:22.04

RUN mkdir -p /workspaces
WORKDIR /workspaces

RUN rm -rf /var/lib/apt/lists/* && apt-get update

RUN apt-get install -y \
    git \
    wget \
    curl \
    zip \
    unzip \
    tar \
    nano \
    cmake \
    build-essential \
    autoconf-archive \
    ninja-build \
    lsb-release \
    software-properties-common \
    gnupg \
    pkg-config

# Set up vcpkg
RUN git clone https://github.com/microsoft/vcpkg
RUN ./vcpkg/bootstrap-vcpkg.sh

# Install LLVM 16
RUN wget https://apt.llvm.org/llvm.sh
RUN chmod +x llvm.sh
RUN ./llvm.sh 16
RUN apt-get install llvm-16

# Install MLIR 16
RUN apt-get install -y libmlir-16-dev mlir-16-tools

# Add user
ARG USERNAME=mikiken
ARG USER_UID=1000
ARG USER_GID=$USER_UID

RUN groupadd --gid $USER_GID $USERNAME \
    && useradd --uid $USER_UID --gid $USER_GID -m $USERNAME \
    && apt-get update \
    && apt-get install -y sudo \
    && echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USERNAME \
    && chmod 0440 /etc/sudoers.d/$USERNAME
USER mikiken