--list images:
sudo docker images

--list containers:
sudo docker ps -a


--delete exited containers:
sudo docker rm $(sudo docker ps -a -q -f status=exited)

--delete images:
sudo docker rmi <IMAGE_ID>


--build image:
sudo docker build -t eshcom/httpsrv .

--push image:
sudo docker push eshcom/httpsrv

--run image:
sudo docker run -it -p 9000:9000 eshcom/httpsrv
