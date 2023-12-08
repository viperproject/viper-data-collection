create schema programs;
create table if not exists "programs"."UserSubmissions" ("submissionId" BIGSERIAL NOT NULL PRIMARY KEY,"submissionDate" TIMESTAMP NOT NULL,"originalName" VARCHAR NOT NULL,"program" VARCHAR NOT NULL,"loc" INTEGER NOT NULL,"frontend" VARCHAR NOT NULL,"argsBlob" BYTEA NOT NULL,"originalVerifier" VARCHAR NOT NULL,"success" BOOLEAN NOT NULL,"runtime" BIGINT NOT NULL);
create table if not exists "programs"."ProgramEntries" ("programEntryId" BIGSERIAL NOT NULL PRIMARY KEY,"submissionDate" TIMESTAMP NOT NULL,"originalName" VARCHAR NOT NULL,"program" VARCHAR NOT NULL,"loc" INTEGER NOT NULL,"frontend" VARCHAR NOT NULL,"originalVerifier" VARCHAR NOT NULL,"args" BYTEA NOT NULL,"originalRuntime" BIGINT NOT NULL,"parseSuccess" BOOLEAN NOT NULL);
create table if not exists "programs"."SiliconResults" ("silResId" BIGSERIAL NOT NULL PRIMARY KEY,"creationDate" TIMESTAMP NOT NULL,"verifierHash" VARCHAR NOT NULL,"programEntryId" BIGINT NOT NULL,"success" BOOLEAN NOT NULL,"didTimeout" BOOLEAN NOT NULL,"runtime" BIGINT NOT NULL,"errors" BYTEA NOT NULL,"phaseRuntimes" BYTEA NOT NULL);
create table if not exists "programs"."CarbonResults" ("carbResId" BIGSERIAL NOT NULL PRIMARY KEY,"creationDate" TIMESTAMP NOT NULL,"verifierHash" VARCHAR NOT NULL,"programEntryId" BIGINT NOT NULL,"success" BOOLEAN NOT NULL,"didTimeout" BOOLEAN NOT NULL,"runtime" BIGINT NOT NULL,"errors" BYTEA NOT NULL,"phaseRuntimes" BYTEA NOT NULL);
create table if not exists "programs"."ProgramPrintEntry" ("pprintID" BIGSERIAL NOT NULL PRIMARY KEY,"programEntryId" BIGINT NOT NULL,"programPrint" BYTEA NOT NULL);
create table if not exists "programs"."Features" ("name" VARCHAR NOT NULL PRIMARY KEY);
create table if not exists "programs"."SiliconFeatureEntries" ("silFeatureEntryId" BIGSERIAL NOT NULL PRIMARY KEY,"featureName" VARCHAR NOT NULL,"resultId" BIGINT NOT NULL,"value" VARCHAR NOT NULL);
create table if not exists "programs"."CarbonFeatureEntries" ("carbFeatureEntryId" BIGSERIAL NOT NULL PRIMARY KEY,"featureName" VARCHAR NOT NULL,"resultId" BIGINT NOT NULL,"value" VARCHAR NOT NULL);

alter table "programs"."SiliconResults" drop constraint if exists "silPE_FK";
alter table "programs"."CarbonResults" drop constraint if exists "carbPE_FK";
alter table "programs"."ProgramPrintEntry" drop constraint if exists "pprintPE_FK";
alter table "programs"."SiliconFeatureEntries" drop constraint if exists "sfeF_FK";
alter table "programs"."SiliconFeatureEntries" drop constraint if exists "sfeSR_FK";
alter table "programs"."CarbonFeatureEntries" drop constraint if exists "cfeF_FK";
alter table "programs"."CarbonFeatureEntries" drop constraint if exists "cfeSR_FK";

alter table "programs"."SiliconResults" add constraint "silPE_FK" foreign key("programEntryId") references "programs"."ProgramEntries"("programEntryId") on update CASCADE on delete CASCADE;
alter table "programs"."CarbonResults" add constraint "carbPE_FK" foreign key("programEntryId") references "programs"."ProgramEntries"("programEntryId") on update CASCADE on delete CASCADE;
alter table "programs"."ProgramPrintEntry" add constraint "pprintPE_FK" foreign key("programEntryId") references "programs"."ProgramEntries"("programEntryId") on update CASCADE on delete CASCADE;
alter table "programs"."SiliconFeatureEntries" add constraint "sfeF_FK" foreign key("featureName") references "programs"."Features"("name") on update CASCADE on delete CASCADE;
alter table "programs"."SiliconFeatureEntries" add constraint "sfeSR_FK" foreign key("resultId") references "programs"."SiliconResults"("silResId") on update CASCADE on delete CASCADE;
alter table "programs"."CarbonFeatureEntries" add constraint "cfeF_FK" foreign key("featureName") references "programs"."Features"("name") on update CASCADE on delete CASCADE;
alter table "programs"."CarbonFeatureEntries" add constraint "cfeSR_FK" foreign key("resultId") references "programs"."CarbonResults"("carbResId") on update CASCADE on delete CASCADE
