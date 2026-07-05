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
render widget = case widget of
    AnyLabel WLabel{..} -> do
        textValue <- evalExpression _labelText
        cls <- mapM evalExpression _labelClasses
        RenderedLabel{..} <- renderLabel RenderLabelProps{renderLabel_text = textValue, renderLabel_class = cls}
        mountExpression _labelText label_setText
        whenJust _labelClasses (void . (`mountExpression` label_setClass))
        return label_widget
    AnyBox WBox{..} -> do
        renderedChildren <- traverse render _boxChildren
        se <- mapM evalExpression _boxSpaceEvenly
        cls <- mapM evalExpression _boxClasses
        o <- mapM evalExpression _boxOrientation
        ha <- mapM evalExpression _boxHalign
        va <- mapM evalExpression _boxValign
        sp <- mapM evalExpression _boxSpacing
        R.RenderedBox{..} <-
            renderBox
                RenderBoxProps
                    { renderBox_spaceEvenly = se
                    , renderBox_children = renderedChildren
                    , renderBox_class = cls
                    , renderBox_orientation = o
                    , renderBox_halign = ha
                    , renderBox_valign = va
                    , renderBox_spacing = sp
                    }
        whenJust _boxSpaceEvenly (void . (`mountExpression` box_setSpaceEvenly))
        whenJust _boxClasses (void . (`mountExpression` box_setClass))
        return box_widget
    AnyButton WButton{..} -> do
        renderedChild <- traverse render _buttonChild
        cls <- mapM evalExpression _buttonClasses
        RenderedButton{..} <-
            maybe
                (renderButton RenderButtonProps{renderButton_child = renderedChild, renderButton_onClick = Nothing, renderButton_class = cls})
                (\(Action action) -> renderButton RenderButtonProps{renderButton_child = renderedChild, renderButton_onClick = Just action, renderButton_class = cls})
                _buttonOnClick
        whenJust _buttonClasses (void . (`mountExpression` button_setClass))
        return button_widget
    AnyCenterBox WBCenterBox{..} -> do
        renderedChildLeft <- traverse render _centerBoxChildLeft
        renderedChildRight <- traverse render _centerBoxChildRight
        renderedChildCenter <- traverse render _centerBoxChildCenter
        cls <- mapM evalExpression _centerBoxClasses
        o <- mapM evalExpression _centerBoxOrientation
        RenderedCenterBox{..} <-
            renderCenterBox
                RenderCenterBoxProps
                    { renderCenterBox_childLeft = renderedChildLeft
                    , renderCenterBox_childRight = renderedChildRight
                    , renderCenterBox_childCenter = renderedChildCenter
                    , renderCenterBox_class = cls
                    , renderCenterBox_orientation = o
                    }
        whenJust _centerBoxClasses (void . (`mountExpression` centerbox_setClass))
        return centerbox_widget

renderWindow :: (RenderingBackend b) => R.BackendHandle b -> Window -> MercuryRuntime b (R.Window b)
renderWindow handle Window{..} = do
    widget <- render rootWidget
    createWindow handle widget geometry title
