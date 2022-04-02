# PyTorch LTS 1.8.2 + CUDA 11.1 + Python 3.9.11 + pyenv
# https://download.pytorch.org/whl/lts/1.8/torch_lts.html

echo "CUDA Toolkit 11.5.2"
wget https://developer.download.nvidia.com/compute/cuda/11.5.2/local_installers/cuda_11.5.2_495.29.05_linux.run
export TERM=xterm
sudo sh cuda_11.5.2_495.29.05_linux.run --silent --toolkit

echo "cuDNN 8.3.3"
wget https://developer.nvidia.com/compute/cudnn/secure/8.3.3/local_installers/11.5/cudnn-linux-x86_64-8.3.3.40_cuda11.5-archive.tar.xz
tar -xvf cudnn-linux-x86_64-8.3.3.40_cuda11.5-archive.tar.xz
sudo cp cudnn-*-archive/include/cudnn*.h /usr/local/cuda/include
sudo cp -P cudnn-*-archive/lib/libcudnn* /usr/local/cuda/lib64
sudo chmod a+r /usr/local/cuda/include/cudnn*.h /usr/local/cuda/lib64/libcudnn*

echo ".bash_aliases"
cat <<'EOF' >>$HOME/.bash_aliases

# CUDA Toolkit
export CUDA_VERSION=11.5
if [ -d "/usr/local/cuda-$CUDA_VERSION" ]; then
    export PATH="/usr/local/cuda-$CUDA_VERSION/bin:$PATH"
    export LD_LIBRARY_PATH="/usr/local/cuda-$CUDA_VERSION/lib64:$LD_LIBRARY_PATH"
fi

# Python venv
if [ -d "$HOME/.venv" ]; then
    source $HOME/.venv/bin/activate
fi
EOF

# Python 3.8.10
sudo apt update
sudo apt install -y python3 python3-pip python3-venv

# mv ~/.pyenv ~/.pyenv.bak
python3 -m venv .venv
source .venv/bin/activate
pip install -U pip
pip install wheel

echo "Tensorflow 2.8.0"
pip install tensorflow tensorflow-addons tqdm opencv-contrib-python

echo "Check CUDA devices"
python -c "from tensorflow.python.client import device_lib; device_lib.list_local_devices()"
