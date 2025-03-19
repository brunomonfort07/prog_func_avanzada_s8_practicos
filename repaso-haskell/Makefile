DOCKER_IMAGE ?= contendedor-pfunc

clean:
	rm -f *~

docker-build:
	make clean
	docker build -t ${DOCKER_IMAGE} .

docker-terminal:
	docker run -it -v .:/pfunc ${DOCKER_IMAGE} bash


