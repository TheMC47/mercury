{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Mercury.Runtime.Rendering (
    mountExpression,
    render,
    renderWindow,
) where

import Control.Monad (void)
import Data.GI.Base.Utils (whenJust)
import Data.Set qualified as S
import Mercury.Runtime
import Mercury.Runtime.Identified
import Mercury.Runtime.Rendering.Backend hiding (Widget, Window)
import Mercury.Runtime.Rendering.Backend qualified as R
import Mercury.Widget
import Mercury.Window

-- TODO collapse [Identified (MercuryRuntime b ())] into Identified (MercuryRuntime b ())
mountExpression :: Expression a -> (a -> MercuryRuntime b ()) -> MercuryRuntime b [Identified (MercuryRuntime b ())]
mountExpression expr onChange =
    traverse
        ( \var ->
            subscribeToVariable var $ evalExpression expr >>= onChange
        )
        (S.toList (dependencies expr))

render :: (R.RenderingBackend b) => Widget -> MercuryRuntime b (R.Widget b)
render (AnyWidget widget) = case widget of
    WLabel{..} -> do
        textValue <- evalExpression label_text
        cls <- mapM evalExpression label_class
        RenderedLabel{..} <- renderLabel RenderLabelProps{renderLabel_text = textValue, renderLabel_class = cls}
        mountExpression label_text label_setText
        whenJust label_class (void . (`mountExpression` label_setClass))
        return label_widget
    WBox{..} -> do
        renderedChildren <- traverse render box_children
        se <- mapM evalExpression box_spaceEvenly
        cls <- mapM evalExpression box_class
        o <- mapM evalExpression box_orientation
        R.RenderedBox{..} <-
            renderBox
                RenderBoxProps
                    { renderBox_spaceEvenly = se
                    , renderBox_children = renderedChildren
                    , renderBox_class = cls
                    , renderBox_orientation = o
                    }
        whenJust box_spaceEvenly (void . (`mountExpression` box_setSpaceEvenly))
        whenJust box_class (void . (`mountExpression` box_setClass))
        return box_widget
    WButton{..} -> do
        renderedChild <- traverse render button_child
        cls <- mapM evalExpression button_class
        RenderedButton{..} <-
            maybe
                (renderButton RenderButtonProps{renderButton_child = renderedChild, renderButton_onClick = Nothing, renderButton_class = cls})
                (\(Action action) -> renderButton RenderButtonProps{renderButton_child = renderedChild, renderButton_onClick = Just action, renderButton_class = cls})
                button_onClick
        whenJust button_class (void . (`mountExpression` button_setClass))
        return button_widget
    WBCenterBox{..} -> do
        renderedChildLeft <- traverse render centerbox_childLeft
        renderedChildRight <- traverse render centerbox_childRight
        renderedChildCenter <- traverse render centerbox_childCenter
        cls <- mapM evalExpression centerbox_class
        o <- mapM evalExpression centerbox_orientation
        RenderedCenterBox{..} <-
            renderCenterBox
                RenderCenterBoxProps
                    { renderCenterBox_childLeft = renderedChildLeft
                    , renderCenterBox_childRight = renderedChildRight
                    , renderCenterBox_childCenter = renderedChildCenter
                    , renderCenterBox_class = cls
                    , renderCenterBox_orientation = o
                    }
        whenJust centerbox_class (void . (`mountExpression` centerbox_setClass))
        return centerbox_widget

renderWindow :: (RenderingBackend b) => R.BackendHandle b -> Window -> MercuryRuntime b (R.Window b)
renderWindow handle Window{..} = do
    widget <- render rootWidget
    createWindow handle widget geometry title
