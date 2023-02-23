# Enter script code
import subprocess
from time import time

wait = 1
start = time()
count = 0
while True:
    count += 1
    result = subprocess.run(["fcitx-remote"], capture_output=True, text=True)
    status = result.stdout.strip()
    if status == "2":
        subprocess.run(["fcitx-remote", "-c"])
    else:
        break
    if time() - start > wait:
        break
#print(f"{status=} {count=}")

keyboard.send_keys("<escape>")