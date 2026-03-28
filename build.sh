docker build --platform linux/amd64 -t datahaskell/sabela .
docker tag datahaskell/sabela:latest 768860981221.dkr.ecr.us-east-1.amazonaws.com/datahaskell/sabela:latest
docker push 768860981221.dkr.ecr.us-east-1.amazonaws.com/datahaskell/sabela:latest
