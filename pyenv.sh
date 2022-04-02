# PyTorch LTS 1.8.2 + CUDA 11.1 + Python 3.9.11 + pyenv
# https://download.pytorch.org/whl/lts/1.8/torch_lts.html

# pyenv
# https://github.com/pyenv/pyenv-installer
curl https://pyenv.run | bash

# .bash_aliases:
# pyenv
if [ -d "$HOME/.pyenv" ]; then
    export PYENV_ROOT="$HOME/.pyenv"
    export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init --path)"
    eval "$(pyenv virtualenv-init -)"
fi
# CUDA Toolkit
export MY_CUDA_VERSION=11.1
if [ -d "/usr/local/cuda-$MY_CUDA_VERSION" ]; then
    export PATH="/usr/local/cuda-$MY_CUDA_VERSION/bin:$PATH"
    export LD_LIBRARY_PATH="/usr/local/cuda-$MY_CUDA_VERSION/lib64:$LD_LIBRARY_PATH"

exec $SHELL

# Python source
wget https://www.python.org/ftp/python/3.9.11/Python-3.9.11.tar.xz
mkdir ~/.pyenv/cache
ln -s Python-3.9.11.tar.xz ~/.pyenv/cache/
pyenv install 3.9.11
pyenv global 3.9.11
pyenv virtualenv 3.9.11 torch182
pyenv global torch182

# PyTorch + torchvision
wget https://download.pytorch.org/whl/lts/1.8/cu111/torch-1.8.2%2Bcu111-cp39-cp39-linux_x86_64.whl
wget https://download.pytorch.org/whl/lts/1.8/cu111/torchvision-0.9.2%2Bcu111-cp39-cp39-linux_x86_64.whl
pip install torch-1.8.2+cu111-cp39-cp39-linux_x86_64.whl torchvision-0.9.2+cu111-cp39-cp39-linux_x86_64.whl

# CUDA
wget https://developer.download.nvidia.com/compute/cuda/11.1.1/local_installers/cuda_11.1.1_455.32.00_linux.run
sudo sh cuda_11.1.1_455.32.00_linux.run
# install Toolkit only

# cuDNN
wget https://developer.nvidia.com/compute/machine-learning/cudnn/secure/8.2.1.32/11.3_06072021/cudnn-11.3-linux-x64-v8.2.1.32.tgz
tar -xzvf cudnn-11.3-linux-x64-v8.2.1.32.tgz
sudo cp cuda/include/cudnn*.h /usr/local/cuda/include
sudo cp -P cuda/lib64/libcudnn* /usr/local/cuda/lib64
sudo chmod a+r /usr/local/cuda/include/cudnn*.h /usr/local/cuda/lib64/libcudnn*
