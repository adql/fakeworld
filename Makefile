include .env.defaults

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

run-psql:
	docker exec -it fakeworld-postgres psql -U postgres

# Run PostgresSQL in a container and (re)setup the database schema
# (OVERRIDES EXISTING TABLES)
set-db:
	make run-db; sleep 1
	docker cp $(DB_SCHEMA_FILE) $(DB_CONTAINER_NAME):/home/
	docker exec $(DB_CONTAINER_NAME) psql -U postgres -f home/schema.sql

# (Re)setup database and populate with dummy data (OVERRIDES EXISTING
# TABLES)
populate-db:
	make set-db; sleep 1
	docker cp db_dummy $(DB_CONTAINER_NAME):/var/lib/postgresql/data/
	docker cp populate.sql $(DB_CONTAINER_NAME):/home/
	docker exec $(DB_CONTAINER_NAME) psql -U postgres -f home/populate.sql

.PHONY: run run-external run-server docker-build docker-run run-db kill-db run-psql set-db populate-db

