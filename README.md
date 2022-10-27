# Terminal
```
/usr/bin/tic -x -o ~/.terminfo xterm-24bit.terminfo
export TERM=xterm-24bit
```

# AutoKey
```
My Phrases>Terminal>
ctrl+semicolon: <ctrl>+/
shift+space: <page_up>
```

# Docker
```
# https://docs.nvidia.com/cuda/cuda-toolkit-release-notes/index.html
# >=496.13
docker run -it --rm --gpus all --ipc=host --ulimit memlock=-1 --ulimit stack=67108864 nvcr.io/nvidia/tensorflow:21.12-tf2-py3
# >=511.65
docker run -it --rm --gpus all --ipc=host --ulimit memlock=-1 --ulimit stack=67108864 nvcr.io/nvidia/tensorflow:22.03-tf2-py3
python -c "from tensorflow.python.client import device_lib; device_lib.list_local_devices()"
```
