FROM centos
# INSTALL JAVA FOR EXECUTION
RUN yum install java-1.8.0-openjdk-devel -y
# INSTALL MAVEN AND GIT FOR S2I
RUN yum install git maven openssh-server openssh-clients -y

# INSTALL COBC
RUN yum install autoconf \
                gcc \
                make \
                libdb-dev \
                libncurses5-dev \
                libgmp-dev \
                gmp gmp-devel \
                which \
                wget \
                -y
RUN wget -O gnu-cobol.tar.gz https://downloads.sourceforge.net/project/open-cobol/gnu-cobol/2.2/gnucobol-2.2.tar.gz
RUN tar zxf gnu-cobol.tar.gz
WORKDIR gnucobol-2.2
RUN ./configure --without-db
RUN make
RUN make install
RUN make check
RUN make installcheck
COPY usrlocallib.conf /etc/ld.so.conf.d/usrlocallib.conf
RUN ldconfig -v

WORKDIR /cobcserver
COPY pom.xml /cobcserver/pom.xml
RUN mvn dependency:go-offline

# BUILD CODE
COPY . /cobcserver
RUN mvn clean package

RUN mkdir -p scripts && chmod -R 0777 scripts
COPY build.sh build.sh

# Run the application
EXPOSE 8080

RUN useradd -ms /bin/bash cobc
USER cobc

CMD ["java", "-jar", "/cobcserver/target/cobc-server.jar"]