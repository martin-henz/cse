#!/bin/bash
for file in *.sml; do echo "use \"$file\"; quit();" | mosml; done

