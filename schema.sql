DROP TABLE IF EXISTS profile, article, comment;

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
       description TEXT NOT NULL,
       body TEXT NOT NULL,
       tag_list VARCHAR(64) [],
       created_at TIMESTAMPTZ NOT NULL,
       updated_at TIMESTAMPTZ NOT NULL CHECK (updated_at >= created_at),
       author_id INT NOT NULL,
       FOREIGN KEY (author_id)
         REFERENCES profile (profile_id)
);

CREATE TABLE comment(
       comment_id SERIAL PRIMARY KEY,
       created_at TIMESTAMP NOT NULL,
       updated_at TIMESTAMPTZ NOT NULL CHECK (updated_at >= created_at),
       body TEXT NOT NULL,
       author_id INT NOT NULL,
       article_id INT NOT NULL,
       FOREIGN KEY (author_id)
         REFERENCES profile (profile_id),
       FOREIGN KEY (article_id)
         REFERENCES article (article_id)
);
