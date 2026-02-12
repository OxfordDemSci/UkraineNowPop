bind = "0.0.0.0:8000"

# Workers / concurrency
workers = 2  # 1 per CPU core
worker_class = "gthread"
threads = 2  # small, safe increase in concurrency

# Timeouts
timeout = 120

# Memory safety
max_requests = 1000
max_requests_jitter = 50

# Logging
accesslog = "-"
errorlog = "-"
loglevel = "info"

# Preload app code (saves memory, faster startup)
preload_app = True
