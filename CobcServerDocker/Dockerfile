FROM centos
RUN yum install java-1.8.0-openjdk-devel -y
RUN yum install git maven openssh-server openssh-clients -y
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

ENV GIT_URL https://github.com/TW5860/CobcServer.git
ARG ssh_prv_key
ARG ssh_pub_key

# Authorize SSH Host
RUN mkdir -p /root/.ssh && \
    chmod 0700 /root/.ssh && \
    # ADD YOUR OWN REPO HOSTER HERE
    ssh-keyscan github.com > /root/.ssh/known_hosts

## Add the keys and set permissions
## USE THIS IF YOU WANT TO RUN WITH BUILD ARGUMENTS
#RUN echo "$ssh_prv_key" > /root/.ssh/id_rsa && \
#    echo "$ssh_pub_key" > /root/.ssh/id_rsa.pub && \
#    chmod 600 /root/.ssh/id_rsa && \
#    chmod 600 /root/.ssh/id_rsa.pub

RUN git clone $GIT_URL cobcserver
RUN chmod -R 775 cobcserver
# Remove SSH keys
RUN rm -rf /root/.ssh/

## Buildprocess
WORKDIR /cobcserver
RUN mvn clean package -DskipTests && pwd && ls -la

# Setup Application context
WORKDIR /
RUN mkdir /cobcserver

WORKDIR /cobcserver
COPY /cobcserver/target/cobc-server.jar app.jar
RUN mkdir scripts && chmod -R 0777 scripts
COPY build.sh build.sh

# Run the application
EXPOSE 8080

RUN useradd -ms /bin/bash cobc
USER cobc

CMD ["java", "-jar", "app.jar"]
#------------------------------------------------------------------------------