# Dockerfile.nginx
FROM nginx:latest

# Copy your HTML/entrypoint files into the image
COPY ./public_html /var/www/public_html

RUN echo "=== Listing copied files ===" && ls -la /var/www/public_html

# Make sure the entrypoint script is executable
RUN chmod +x /var/www/public_html/entrypoint.sh

# Use your custom entrypoint
CMD ["/bin/sh", "-c", "/var/www/public_html/entrypoint.sh"]
