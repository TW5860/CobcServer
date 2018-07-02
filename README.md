Use this to create a OC yaml

    brew install kompose

    kompose convert -f docker-compose.yml --provider openshift --build  build-config --build-repo https://github.com/TW5860/CobcServer.git -o cobc-server.yaml

    oc create -f cobc-server.yaml
