DROP TABLE IF EXISTS profile, article;

CREATE TABLE profile(
       profile_id SERIAL PRIMARY KEY,
       username VARCHAR(64) NOT NULL,
       bio TEXT,
       image TEXT
);

CREATE TABLE article(
       article_id SERIAL PRIMARY KEY,
       slug TEXT UNIQUE NOT NULL,
       title TEXT NOT NULL,
       description TEXT,
       body TEXT NOT NULL,
       tag_list VARCHAR(64) [],
       created_at TIMESTAMPTZ NOT NULL,
       updated_at TIMESTAMPTZ CHECK (updated_at > created_at),
       author_id INT NOT NULL,
       FOREIGN KEY (author_id)
         REFERENCES profile (profile_id)
);
