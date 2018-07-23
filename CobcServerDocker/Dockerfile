###############################################################################
# BASE IMAGE FOR ALL
FROM centos AS javaServer
RUN yum install java-1.8.0-openjdk-devel -y
#------------------------------------------------------------------------------

###############################################################################
# BASE IMAGE FOR JAVA BUILD CONTAINER WITH MAVEN
FROM javaServer AS javaBuildServer
RUN yum install git maven openssh-server openssh-clients -y
#------------------------------------------------------------------------------

###############################################################################
# MACHINE WITH COBC INSTALLED
FROM javaServer AS cobcBuild

# Setup operating system and required applications
RUN yum install autoconf \
                gcc \
                make \
                libdb-dev \
                libncurses5-dev \
                libgmp-dev \
                gmp gmp-devel \
                which \
                -y
COPY gnu-cobol.tar.gz gnu-cobol.tar.gz
RUN tar zxf gnu-cobol.tar.gz

# Install cobc
WORKDIR gnucobol-2.2
RUN ./configure --without-db
RUN make
RUN make install
RUN make check
RUN make installcheck
COPY usrlocallib.conf /etc/ld.so.conf.d/usrlocallib.conf
RUN ldconfig -v
#------------------------------------------------------------------------------

###############################################################################
# JAVA SOURCE TO IMAGE FROM REPO
FROM javaBuildServer AS javaBuild
ENV GIT_URL git@github.com:TW5860/CobcServer.git
ARG ssh_prv_key
ARG ssh_pub_key

# Authorize SSH Host
RUN mkdir -p /root/.ssh && \
    chmod 0700 /root/.ssh && \
    # ADD YOUR OWN REPO HOSTER HERE
    ssh-keyscan github.com > /root/.ssh/known_hosts

# Add the keys and set permissions
RUN echo "$ssh_prv_key" > /root/.ssh/id_rsa && \
    echo "$ssh_pub_key" > /root/.ssh/id_rsa.pub && \
    chmod 600 /root/.ssh/id_rsa && \
    chmod 600 /root/.ssh/id_rsa.pub

RUN git clone $GIT_URL cobcserver && chmod -R 775 /cobcserver
# Remove SSH keys
RUN rm -rf /root/.ssh/

## Buildprocess
WORKDIR /cobcserver
RUN mvn clean package -DskipTests
#------------------------------------------------------------------------------


###############################################################################
# DEPLOYABLE IMAGE COBC + JAVA COMPILED
FROM cobcBuild
# Setup Application context
WORKDIR /
RUN mkdir /cobcserver

WORKDIR /cobcserver
COPY --from=javaBuild /cobcserver/target/cobc-server.jar app.jar
RUN mkdir scripts && chmod -R 0777 scripts
COPY build.sh build.sh

# Run the application
EXPOSE 8080

RUN useradd -ms /bin/bash cobc
USER cobc

CMD ["java", "-jar", "app.jar"]
#------------------------------------------------------------------------------