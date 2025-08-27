import sys
import torch.nn as nn
from classifier import _Classifier
from data import Data
from model import Model
from ae1 import AE1
from ae2 import AE2
from ae3 import AE3

class CL1(_Classifier):
    def __init__(self, path):
        super().__init__(path)
        n_kernels = 64

        ae1 = AE1('models/ae1.pt'); ae1.load()
        ae2 = AE2('models/ae2.pt'); ae2.load()
        ae3 = AE3('models/ae3.pt'); ae3.load()
        encoder1 = ae1.encoder
        encoder2 = ae2.encoder
        encoder3 = ae3.encoder

        self.model = Model(
            input_shape=(self.BATCH_SIZE, 3, 64, 64),
            layers=[
                encoder1,
                encoder2,
                encoder3,
                nn.Flatten(),
                nn.Dropout(p=0.1),
                nn.Linear(n_kernels * 8 * 8, 256),  # 64*8*8 = 4096 inputs
                nn.ReLU(),
                nn.Dropout(p=0.1),
                nn.Linear(256, 64),
                nn.ReLU(),
                nn.Linear(64, 4)
            ]
        )

if __name__ == '__main__':
    epochs = int(sys.argv[1]) if len(sys.argv) > 1 else None
    data = Data.load('data', image_size=64)
    data.shuffle()
    cl1 = CL1('models/cl1.pt')
    cl1.print()
    if not epochs:
        print(f'\nLoading {cl1.path}...')
        cl1.load()
    else:
        train_data, test_data = data.split(0.8)
        print(f'\nTraining classifier for {epochs} epochs...')
        cl1.train(epochs, train_data, test_data)
        print(f'\nSaving model to {cl1.path}...')
        cl1.save()
    
    results = cl1.classify(data)    # Evaluate on the entire dataset
    print(f'\nTotal Dataset Accuracy: {results.accuracy(data):.1f}%')
    print(f'\nConfusion Matrix:\n{results.confusion_matrix(data)}')
    
    results.display(16, data)   # Display some example classifications
