# Run with local server
run:
	cabal run

# Run with external server
run-external:
	cabal run fakeworld -- -e

# Run server only (no Brick front-end)
run-server:
	cabal run fakeworld -- -s

# Build docker image for running the server
docker-build:
	docker build -t adql/fakeworld .

# Run the docker image
docker-run:
	docker run -dp 8000:8000 adql/fakeworld

.PHONY: run run-external run-server docker-build docker-run
