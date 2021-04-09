FROM erlang:21.3.8.22-alpine

EXPOSE 9000

WORKDIR /usr/app/httpsrv
COPY ./ ./

ENTRYPOINT ["./run_test.sh"]
