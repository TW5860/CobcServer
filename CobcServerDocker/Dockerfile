FROM registry.access.redhat.com/rhel7:7.4

# Setup operating system and required applications
RUN yum install wget gcc make libdb-dev libncurses5-dev libgmp-dev gmp gmp-devel autoconf -y
RUN yum install -y java-1.8.0-openjdk-devel wget which
RUN wget -O gnu-cobol.tar.gz https://sourceforge.net/projects/open-cobol/files/gnu-cobol/2.2/gnucobol-2.2.tar.gz/download
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
CMD ["java", "-jar", "/cobcserver/app.jar"]
