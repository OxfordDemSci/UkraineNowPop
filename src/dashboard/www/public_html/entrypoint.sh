#!/bin/sh
cat <<EOF > /var/www/public_html/js/env.js
export function get_api_url() {
    return "$NOWPOP_API_URL";
  }
EOF
nginx -g "daemon off;"