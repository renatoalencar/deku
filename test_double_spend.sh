SIDECLI=$(esy x which sidecli)
sidecli () {
  eval $SIDECLI '"$@"'
}

sidecli create-transaction ./data/0 ./tz1LYANwSGkEPVXicH3a9x35VqxTaCvW4koS.tzsidewallet '{"Action":"Increment"}'
sidecli create-transaction ./data/0 ./tz1LYANwSGkEPVXicH3a9x35VqxTaCvW4koS.tzsidewallet '{"Action":"Decrement"}' &
sidecli create-transaction ./data/1 ./tz1LYANwSGkEPVXicH3a9x35VqxTaCvW4koS.tzsidewallet '{"Action":"Decrement"}' &
wait
