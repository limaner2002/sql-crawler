{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AppianPage where

import ClassyPrelude
import Data.Aeson
import Data.Aeson.Types

data AppianPage
  = Feed Entries
  | UiConfig SailComponent
  | Updates SailComponent
  deriving (Show, Eq)

instance FromJSON AppianPage where
  parseJSON (Object o) = parseFeed <|> parseDocument <|> parseUpdate
    where
      parseFeed = Feed <$> o .: "feed"
      parseDocument = ifAppianType o "UiConfig" (UiConfig <$> o .: "ui")
      parseUpdate = ifAppianType o "UiConfig" (Updates <$> o .: "updates")
  parseJSON v = parseFailure v

newtype Entries = Entries [Entry]
  deriving (Show, Eq)

instance FromJSON Entries where
  parseJSON (Object o) = Entries <$> o .: "entries"
  parseJSON v = parseFailure v

-- newtype ColumnLayout = ColumnLayout [FormLayout]
--   deriving (Show, Eq)

-- instance FromJSON ColumnLayout where
--   parseJSON (Object o) = ifAppianType o "ColumnLayout" (ColumnLayout <$> o .: "contents")
--   parseJSON v = parseFailure v

 -- Possibly separate requests into their own type.
data SailComponent
  = Form Buttons Label SailComponent
  | Align [SailComponent]
  | Columns [SailComponent]
  | SectionLayout SailComponent
  | Field Label -- HelpTooltip
  | GridField Label PagingInfo TotalCount
  | GridTextColumn Data Label
  | ButtonLayout Buttons
  | MilestoneField [Step]
  | GridLayout Text
  | ColumnLayout [SailComponent]
  | DropdownField Choices
  | SaveRequestList [SailComponent]
  | SaveRequestListValue SailComponent
  | GridSelection [AppianValue] PagingInfo
  | PickerField Label
  deriving (Show, Eq)

instance FromJSON SailComponent where
  parseJSON v@(Object o) = if hasType then showIt "withType" withType else showIt "parseAlign" parseAlign <|> showIt "parseColumns" parseColumns <|> showIt "parseRequestListValue" parseSaveRequestListValue <|> showIt "parseField" parseField
    where
      hasType =
        case lookup "#t" o of
          Nothing -> False
          Just _ -> True
      withType = do
        typ <- parseJSON v
        case typ of
          TypFormLayout ->
            Form <$> o .: "buttons"
                 <*> o .: "label"
                 <*> o .: "content"
          TypButtonLayout ->
            ButtonLayout <$> parseJSON v
          TypGridTextColumn ->
            GridTextColumn <$> o .: "data"
                           <*> o .: "label"
          TypSectionLayout ->
            SectionLayout <$> o .: "content"
          TypMilestoneField ->
            MilestoneField <$> o .: "steps"
          TypTextField -> parseField
          TypGridField -> parseGridField
          TypParagraphField -> parseField
          TypGridLayout -> GridLayout <$> o .: "emptyGridMessage"
          TypColumnLayout ->
            ColumnLayout <$> o .: "contents"
          TypDropdownField -> DropdownField <$> o .: "choices"
          TypSaveRequestList ->
            SaveRequestList <$> o .: "#v"
          TypGridSelection ->
            GridSelection <$> o .: "selected"
                          <*> o .: "pagingInfo"
          TypPickerField ->
            PickerField <$> o .: "label"
      parseAlign =
        Align <$> o .: "columns"
      parseColumns =
        Columns <$> o .: "contents"
      parseField =
        Field <$> o .: "label"
      parseGridField =
        GridField <$> o .: "label"
                  <*> o .: "value"
                  <*> o .: "totalCount"
      parseSaveRequestListValue =
        SaveRequestListValue <$> o .: "value"
      showIt :: FromJSON a => String -> Parser a -> Parser a
      -- showIt l parser = trace (l <> ": (" <> show (lookup "#t" o) <> ", " <> show (keys o) <> ")") parser >>= \x -> trace "Succeded!" return x
      showIt l = id
  parseJSON v = parseFailure v

data Buttons = Buttons (Maybe [Button]) (Maybe [Button])
  deriving (Show, Eq)

instance FromJSON Buttons where
  parseJSON (Object o) =
    Buttons <$> o .:? "primaryButtons"
            <*> o .:? "secondaryButtons"
  parseJSON v = parseFailure v

data Button
  = Button SaveInto AppianValue (Maybe Action) (Maybe Validate) Label ConfirmMessage
  deriving (Show, Eq)

instance FromJSON Button where
  parseJSON (Object o) =
    Button <$> o .: "saveInto"
           <*> o .: "value"
           <*> o .:? "action"
           <*> o .:? "validate"
           <*> o .: "label"
           <*> o .: "confirmMessage"
  parseJSON v = parseFailure v

newtype SaveInto = SaveInto [Text]
  deriving (Show, Eq)

instance FromJSON SaveInto where
  parseJSON (Array arr) = SaveInto <$> f
    where
      f = toList <$> mapM parseJSON arr
      
  parseJSON v = parseFailure v

data AppianValue = AppianValue Text SailPrimitiveType
  deriving (Show, Eq)

instance FromJSON AppianValue where
  parseJSON (Object o) =
    AppianValue <$> o .: "#t"
                <*> o .: "#v"
  parseJSON v = parseFailure v

newtype Action = Action Text
  deriving (Show, Eq)

instance FromJSON Action where
  parseJSON v = Action <$> parseJSON v

newtype Validate = Validate Bool
  deriving (Show, Eq)

instance FromJSON Validate where
  parseJSON v = Validate <$> (readLowercaseBool =<< parseJSON v)

newtype Label = Label Text
  deriving (Show, Eq)

instance FromJSON Label where
  parseJSON v = Label <$> parseJSON v

newtype ConfirmMessage = ConfirmMessage Text
  deriving (Show, Eq)

instance FromJSON ConfirmMessage where
  parseJSON v = ConfirmMessage <$> parseJSON v

data Entry = Entry Text EntryContent
  deriving (Show, Eq)

instance FromJSON Entry where
  parseJSON (Object o) =
    Entry <$> o .: "title"
          <*> o .: "content"
  parseJSON v = parseFailure v

newtype EntryContent = EntryContent [Text]
  deriving (Show, Eq)

instance FromJSON EntryContent where
  parseJSON (Object o) = EntryContent <$> o .: "children"
  parseJSON v = parseFailure v

newtype HelpTooltip = HelpTooltip Text
  deriving (Show, Eq)

instance FromJSON HelpTooltip where
  parseJSON v = HelpTooltip <$> parseJSON v

newtype Data = Data [Text]
  deriving (Show, Eq)

instance FromJSON Data where
  parseJSON v = Data <$> parseJSON v

newtype Step = Step Text
  deriving (Show, Eq)

instance FromJSON Step where
  parseJSON v = Step <$> parseJSON v

newtype Choices = Choices [Text]
  deriving (Show, Eq)

instance FromJSON Choices where
  parseJSON v = Choices <$> parseJSON v

data AppianType
  = TypGridTextColumn
  | TypFormLayout
  | TypSectionLayout
  | TypButtonLayout
  | TypMilestoneField
  | TypTextField
  | TypGridField
  | TypParagraphField
  | TypGridLayout
  | TypColumnLayout
  | TypDropdownField
  | TypSaveRequestList
  | TypGridSelection
  | TypPickerField
  deriving (Show, Eq, Read)

instance FromJSON AppianType where
  parseJSON (Object o) = do
    v <- o .: "#t" :: Parser Text
    case textToAppianType v of
      Nothing -> fail $ show v <> " is not a valid Appian type."
      Just typ -> return typ
  parseJSON v = parseFailure v


textToAppianType :: Text -> Maybe AppianType
textToAppianType "GridTextColumn" = Just TypGridTextColumn
textToAppianType "FormLayout" = Just TypFormLayout
textToAppianType "SectionLayout" = Just TypSectionLayout
textToAppianType "ButtonLayout" = Just TypButtonLayout
textToAppianType "MilestoneField" = Just TypMilestoneField
textToAppianType "TextField" = Just TypTextField
textToAppianType "GridField" = Just TypGridField
textToAppianType "ParagraphField" = Just TypParagraphField
textToAppianType "GridLayout" = Just TypGridLayout
textToAppianType "ColumnLayout" = Just TypColumnLayout
textToAppianType "DropdownField" = Just TypDropdownField
textToAppianType "SaveRequest?list" = Just TypSaveRequestList
textToAppianType "GridSelection" = Just TypGridSelection
textToAppianType "PickerField" = Just TypPickerField
textToAppianType _ = Nothing

parseFailure :: (Monad m, Show a1) => a1 -> m a
parseFailure x = fail $ "Expected boolean, got '" <> show x <> "'"

readLowercaseBool :: Monad m => String -> m Bool
readLowercaseBool "true" = return True
readLowercaseBool "false" = return False
readLowercaseBool str = parseFailure str

ifAppianType :: HashMap Text Value -> Text -> Parser a -> Parser a
ifAppianType o typ p = do
  objectType <- o .: "#t"
  case objectType == typ of
    True -> p
    False -> fail . unpack $ "Expected '" <> typ <> "' but got '" <> objectType <> "'"

data SailPrimitiveType
  = SailInt Int
  | SailText Text
  deriving (Show, Eq)

instance FromJSON SailPrimitiveType where
  parseJSON v =
        SailInt <$> parseJSON v
    <|> SailText <$> parseJSON v

data PagingInfo = PagingInfo BatchSize StartIndex
  deriving (Show, Eq)

instance FromJSON PagingInfo where
  parseJSON (Object o) =
    PagingInfo <$> o .: "batchSize"
               <*> o .: "startIndex"
  parseJSON v = parseFailure v

newtype BatchSize = BatchSize Int
  deriving (Show, Eq, FromJSON)

newtype StartIndex = StartIndex Int
  deriving (Show, Eq, FromJSON)

newtype TotalCount = TotalCount Int
  deriving (Show, Eq, FromJSON)
