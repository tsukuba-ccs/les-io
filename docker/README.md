# Docker containers to test LESBENCHMARK

## How to use

- build image

      % docker compose build

- execute containers

      % docker compose up -d

  This executes four contaiers using the docker image.  You can login to all containers by ssh.  For details, see docker-compose.yml.

- login to a container and execute LESBENCHMARK

      % ssh 172.30.1.2
      % mpirun -np 4 -hostfile hosts -map-by node LESBENCHMARK

- shutdown containers

      % docker compose down
