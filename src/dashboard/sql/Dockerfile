FROM postgres:15.5-bullseye

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        postgis \
        postgresql-15-postgis-3 && \
    rm -rf /var/lib/apt/lists/*