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
RUN yum install git maven java-1.8.0-openjdk-devel -y
RUN git clone https://github.com/TW5860/CobcServer.git
#COPY cobc-server.jar /cobcserver/app.jar
COPY build.sh /CobcServer/build.sh
RUN chmod -R 775 /CobcServer


# Run the application
WORKDIR /CobcServer
RUN mvn clean package -DskipTests
RUN mv target/cobc-server.jar app.jar

RUN touch testdriver.cbl
RUN touch test
RUN touch ../test
EXPOSE 8080

# Add cobc user
RUN useradd -ms /bin/bash cobc
USER cobc

CMD ["java", "-jar", "/CobcServer/app.jar"]
