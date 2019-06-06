# nuitomo-api

## Setup
```
docker-compose up -d
make migrate-up
```

### build all components
```
make build-all
```

### set secret key

```
stack exec -- nuitomo-api-batch gen-key
# set generated key to config.yaml
```

## build specific component
```
make build TARGET=api
# or `make build` # api by default
```

## execute installed app
```
stack exec -- nuitomo-api-api
```

## Develop

### API

```
make ghci
> runAppM' $ getTasksR (testUser 0) -- run hander
> serve -- server on http://localhost:8080
```

### lint
#### install hlint
```
stack install hlint
```

#### run
```
make lint
```

### test
```
make test
```

#### run tests with ghci
```
make ghci-test
```
### run specific test
```
> :set args -m "name"
> Spec.main
```

## haddock
```
make doc
```

## Mock Server
```
stack exec -- nuitomo-api-mock
# http://localhost:8082
```


## Swagger
### serve
```
stack exec -- nuitomo-api-swagger ui
# http://localhost:8081/swagger-ui
# http://localhost:8081/swagger.json
```

### output
```
stack exec -- nuitomo-api-swagger
```

## template

### use this project as stack template

```
stack new my-project https://raw.githubusercontent.com/lemois/nuitomo-api/templates/nuitomo-api.hsfiles
cd my-project
chmod +x scripts/*
```
-- TODO: private projectにしている間はこの手順では無理。localに保存して実行。

### update hsfiles

```
./scripts/makeTemplate.sh
git checkout templates
mv nuitomo-api.hsfiles-tmp nuitomo-api.hsfiles
# commit
```