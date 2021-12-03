# AnimeChicago CYOA Client

This is the front-end of the AnimeChicago CYOA app.

## Development

### Initial Setup

Recommended: Create a new [gitpod](https://gitpod.io) workspace from this repository. All of the required setup will be performed and the app will be started automatically.

If you're not using gitpod, or you need to set things up manually, please follow these instructions:

```
git clone https://github.com/sharma7n/animechicago-cyoa-client
cd animechicago-cyoa-client
npm i -g elm elm-format create-elm-app
elm-app start
```

## Deployment

1. Build the application with `elm-app build`. This will create new files in a directory called `build`.
2. (Optional but recommended) Backup the existing files in the AnimeChicago Advice Bot FTP Server.
3. Upload all of the files from `build` to the AnimeChicago Advice Bot FTP Server (overwriting existing files).
