#!/usr/bin/env bash

swipl -f server.pl \
      -g 'search_visualisation_server:start_server(4000)'
