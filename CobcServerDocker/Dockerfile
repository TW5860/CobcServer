FROM centos AS javaBuild
RUN yum install git maven java-1.8.0-openjdk-devel -y
RUN git clone https://github.com/TW5860/CobcServer.git cobcserver && chmod -R 775 /cobcserver
## Buildprocess
WORKDIR /cobcserver
RUN mvn clean package -DskipTests




FROM centos AS cobcBuild

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




FROM cobcBuild
# Setup Application context
WORKDIR /
RUN mkdir /cobcserver

WORKDIR /cobcserver
COPY --from=javaBuild /cobcserver/target/cobc-server.jar app.jar
RUN mkdir scripts
RUN chmod -R 0777 scripts
COPY build.sh build.sh


# Run the application

EXPOSE 8080

RUN useradd -ms /bin/bash cobc
USER cobc

CMD ["java", "-jar", "app.jar"]
#CMD ["sh"]
