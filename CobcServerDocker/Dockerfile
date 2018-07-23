#FROM registry.access.redhat.com/rhel7:7.4
FROM centos

# Setup operating system and required applications
RUN yum install \
                autoconf \
                gcc  \
                gmp gmp-devel \
                java-1.8.0-openjdk-devel \
                libdb-dev \
                libncurses5-dev \
                libgmp-dev \
                make \
                which -y
#RUN wget -O gnu-cobol.tar.gz https://sourceforge.net/projects/open-cobol/files/gnu-cobol/2.2/gnucobol-2.2.tar.gz/download
COPY gnu-cobol.tar.gz gnu-cobol.tar.gz
RUN tar zxf gnu-cobol.tar.gz

# Install cobc
WORKDIR gnucobol-2.2
RUN ./configure --without-db
RUN make
RUN make install
RUN make check
RUN make installcheck
RUN echo "/usr/local/lib > /etc/ld.so.conf.d/usrlocallib.conf"
RUN ldconfig -v

# Setup Application context
WORKDIR /
COPY cobc-server.jar /cobcserver/app.jar
COPY build.sh /cobcserver/build.sh

RUN chmod -R 775 /cobcserver


# Run the application
WORKDIR /cobcserver

RUN touch testdriver.cbl
RUN touch test
RUN touch ../test
EXPOSE 8080

# Add cobc user
RUN groupadd -r cobc && useradd -r -g cobc \
    && mkdir -p /home/cobc/Downloads && chown -R cobc:cobc /home/cobc
# Run Chrome as non privileged user
USER cobc

CMD ["java", "-jar", "/cobcserver/app.jar"]
