# 1. R officielle with GDAL, PROJ, et RStudio
FROM rocker/geospatial:latest

# 2. Installation des dépendances système supplémentaires si besoin
RUN apt-get update && apt-get install -y \
    wget \
    nano \
    && rm -rf /var/lib/apt/lists/*

# 3. Installation de JULIA (Version 1.10 LTS recommandée pour la stabilité)
ENV JULIA_VER=1.10.0
RUN mkdir /opt/julia && \
    wget -q https://julialang-s3.julialang.org/bin/linux/x64/1.10/julia-${JULIA_VER}-linux-x86_64.tar.gz && \
    tar zxvf julia-${JULIA_VER}-linux-x86_64.tar.gz -C /opt/julia --strip-components 1 && \
    rm julia-${JULIA_VER}-linux-x86_64.tar.gz && \
    ln -s /opt/julia/bin/julia /usr/local/bin/julia

# 4. Configuration des variables pour que R trouve Julia
ENV JULIA_BINDIR=/opt/julia/bin

# 5. Pré-installation du paquet R JuliaCall
RUN R -e "install.packages('JuliaCall')"

# 6. Pré-installation des paquets Julia pour gagner du temps au lancement
ENV JULIA_DEPOT_PATH=/opt/julia_depot
RUN mkdir -p $JULIA_DEPOT_PATH && chmod 777 $JULIA_DEPOT_PATH
RUN julia -e 'import Pkg; Pkg.add("RCall"); Pkg.add("Omniscape"); Pkg.add("GDAL"); Pkg.build("RCall")'

# 7. Copy the local package code into the Docker image
COPY DESCRIPTION /build/spacemodR/DESCRIPTION
RUN R -e "remotes::install_deps('/build/spacemodR', dependencies = TRUE)"
COPY . /build/spacemodR
RUN R -e "remotes::install_local('/build/spacemodR', dependencies = FALSE)"
RUN rm -rf /build/spacemodR

# 8. Install spacemodR and its R dependencies
# We use remotes::install_local (already included in rocker/geospatial)
RUN R -e "remotes::install_local('/build/spacemodR', dependencies = TRUE)"

# 9. Clean up the build directory to keep the image size small
RUN rm -rf /build/spacemodR
