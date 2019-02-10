# smith-cli

This is a command line interface for the [Smith](https://smith.st).

The goal is to provide a convenient interface for working with smith.


### Configuration

The smith cli will source credentials configuration as follows:
 - It will check for an environment provided API key in '$SMITH_JWK'.
 - It will fall-back to looking for '$SMITH_HOME/credentials.json' if '$SMITH_HOME' is set.
 - It will fall-back to looking for '$HOME/.smith/credentials.json'.

The smith cli will source endpoint configuration as follows:
 - It will check for an environment provided endpoint in '$SMITH_ENDPOINT'.
 - It will fall-back to the public production endpoint 'https://api.smith.st'.


### Stability

This cli is new, and should have the disclaimers that normally comes
with that. However, the command line aims to maintain compatibility
unless there is a non-small issue that breaking compatibility will
really address. Stable versions will always be available for
downoad/install if you really need to lock things down.


### Example

Using your ssh-agent.
```
# using ssh agent
eval $(ssh-agent)

# issue a certificate for the muppets environment
smith --environment muppets
smith -e muppets
```

Running a command with access to an agent configured with your certificate.
```
# start ssh-agent issue a certificate for the muppets environment
smith --environment muppets -- ssh user@kermit
smith --environment muppets -- rsync -aH www www@gonzo:/var/www
```

### Builds

[![CircleCI](https://circleci.com/gh/smith-security/smith-cli.svg?style=svg)](https://circleci.com/gh/smith-security/smith-cli)
