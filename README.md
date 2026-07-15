# How to run

````bash
cd <docker-folder>
docker compose build
docker compose up
````

To verify the recovery, execute in another terminal:
````bash
cd <docker-folder>
docker compose stop <service>
docker compose start <service>
docker compose logs -f <service>
````

Example:
````bash
cd docker-simple
docker compose stop acu1
docker compose start acu1
docker compose logs -f acu1
````