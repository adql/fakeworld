DB_CONTAINER_NAME=fakeworld-postgres
DB_DOCKER_IMAGE=postgres:15.1-alpine
DB_PORT=5432
DB_POSTGRES_PASSWORD=fakeworld
DB_VOLUME_NAME=fakeworld-postgres

# Run with local server
run:
	make run-db && cabal run

# Run with external server
run-external:
	cabal run fakeworld -- -e

# Run server only (no Brick front-end)
run-server:
	make run-db && cabal run fakeworld -- -s

# Build docker image for running the server
docker-build:
	docker build -t adql/fakeworld .

# # Run the docker image
# docker-run:
# 	docker run -dp 8000:8000 adql/fakeworld

# Run PostgreSQL in a container, for development of the app itself
# without a container
run-db:
	docker ps | grep $(DB_CONTAINER_NAME) || \
	docker run \
	--name $(DB_CONTAINER_NAME) \
	-e POSTGRES_PASSWORD=$(DB_POSTGRES_PASSWORD) \
	-v $(DB_VOLUME_NAME):/var/lib/postgresql/data \
	-p $(DB_PORT):5432 \
	-d \
	$(DB_DOCKER_IMAGE)

kill-db:
	docker rm -f $(DB_CONTAINER_NAME)

.PHONY: run run-external run-server docker-build docker-run run-db kill-db

