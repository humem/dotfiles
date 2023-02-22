#!/bin/bash

IP=$(ifconfig eth0 | grep 'inet ' | awk '{print $2}')

netsh.exe interface portproxy delete v4tov4 listenport=22
netsh.exe interface portproxy add    v4tov4 listenport=22 connectaddress=$IP

sc.exe config iphlpsvc start=auto
sc.exe start  iphlpsvc
