# fcitx-mozc
```
sudo apt install fcitx-mozc
vi .config/fcitx/config
vi .config/fcitx/conf/clipboard
fcitx-configtool
/usr/lib/mozc/mozc_tool --mode=config_dialog
fcitx 2?/dev/null
autokey 2>/dev/null &

# sudo apt install xbindkeys xdotool
# fcitx; xbindkeys &
```
Hit F2 or Ctrl-SPC to enable mozc, and Ctrl-[ to disable.

# Terminal
```
/usr/bin/tic -x -o ~/.terminfo xterm-24bit.terminfo
export TERM=xterm-24bit
```

# AutoKey
```
My Phrases>Terminal>
# ctrl+semicolon: <ctrl>+/
shift+space: <page_up>
ctrl+bracketleft: fcitx-remote -c
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
