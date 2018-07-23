Use this to create a OC yaml on Mac

    brew install kompose

    kompose convert -f docker-compose.yml --provider openshift --build  build-config --build-repo https://github.com/TW5860/CobcServer.git -o cobc-server.yaml

    oc create -f cobc-server.yaml


Otherwise use the 'cobc-server.yaml' included in the project as a template

In order to start the Docker container with ssh you need to use this:
(WARING! It is highly recommended to use the --squash flag aswell. This is currently only available as an experimental feature)


 docker build -t cobc-server --build-arg ssh_prv_key="$(cat <private_key>)" --build-arg ssh_pub_key="$(cat <public_key>)" CobcServerDocker/. 
