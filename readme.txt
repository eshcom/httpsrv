--list images:
docker images

--list containers:
docker ps -a


--delete exited containers:
docker rm $(docker ps -a -q -f status=exited)

--delete images:
docker rmi <IMAGE_ID>


--build image:
docker build -t eshcom/httpsrv .

--push image:
docker push eshcom/httpsrv

--run image:
docker run -it -p 9000:9000 eshcom/httpsrv
